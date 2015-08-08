;;; haskell-process.el --- Communicating with the inferior Haskell process -*- lexical-binding: t -*-

;; Copyright (C) 2011  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-util)
(require 'haskell-compat)
(require 'haskell-session)
(require 'haskell-customize)
(require 'haskell-string)

(defconst haskell-session-prompt-regex "\4"
  "Used for delimiting command replies. 4 is End of Transmission.")

(defvar haskell-reload-p nil
  "Used internally for `haskell-session-loadish'.")

(defconst haskell-session-greetings
  (list "Hello, Haskell!"
        "The lambdas must flow."
        "Hours of hacking await!"
        "The next big Haskell project is about to start!"
        "Your wish is my IO ().")
  "Greetings for when the Haskell process starts up.")

(defconst haskell-session-logo
  (expand-file-name "logo.svg" haskell-mode-pkg-base-dir)
  "Haskell logo for notifications.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing commands -- using cl 'defstruct'

(cl-defstruct haskell-command
  "Data structure representing a command to be executed when with
  a custom state and three callback."
  ;; hold the custom command state
  ;; state :: a
  state
  ;; called when to execute a command
  ;; go :: a -> ()
  go
  ;; called whenever output was collected from the haskell process
  ;; live :: a -> Response -> Bool
  live
  ;; called when the output from the haskell process indicates that the command
  ;; is complete
  ;; complete :: a -> Response -> ()
  complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the process

(defun haskell-session-compute-process-log-and-command (session hptype)
  "Compute the log and process to start command for the SESSION from the HPTYPE.
Do not actually start any process.
HPTYPE is the result of calling `'haskell-session-type`' function."
  (let ((session-name (haskell-session-name session)))
    (cl-ecase hptype
      ('ghci
       (append (list (format "Starting inferior GHCi process %s ..."
                             haskell-session-path-ghci)
                     session-name
                     nil)
               (apply haskell-session-wrapper-function
                      (list
                       (cons haskell-session-path-ghci haskell-session-args-ghci)))))
      ('cabal-repl
       (append (list (format "Starting inferior `cabal repl' process using %s ..."
                             haskell-session-path-cabal)
                     session-name
                     nil)
               (apply haskell-session-wrapper-function
                      (list
                       (append
                        (list haskell-session-path-cabal "repl")
                        haskell-session-args-cabal-repl
                        (let ((target (haskell-session-target session)))
                          (if target (list target) nil)))))))
      ('stack-ghci
       (append (list (format "Starting inferior stack GHCi process using %s" haskell-session-path-stack)
                     session-name
                     nil)
               (apply haskell-session-wrapper-function
                      (list
                       (append
                        (list haskell-session-path-stack "ghci")
                        (let ((target (haskell-session-target session)))
                          (if target (list target) nil))
                        haskell-session-args-stack-ghci))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun haskell-session-sentinel (proc event)
  "The sentinel for the process pipe."
  (let ((session (haskell-session-project-by-proc proc)))
    (when session
      (unless (haskell-session-restarting session)
	(haskell-session-log
	 (propertize (format "Event: %S\n" event)
		     'face '((:weight bold))))
	(haskell-session-log
	 (propertize "Process reset.\n"
		     'face font-lock-comment-face))
	(run-hook-with-args 'haskell-session-ended-hook session)))))

(defun haskell-session-filter (proc response)
  "The filter for the process pipe."
  (let ((i 0))
    (cl-loop for line in (split-string response "\n")
             do (haskell-session-log
                 (concat (if (= i 0)
                             (propertize "<- " 'face font-lock-comment-face)
                           "   ")
                         (propertize line 'face 'haskell-interactive-face-compile-warning)))
             do (setq i (1+ i))))
  (let ((session (haskell-session-project-by-proc proc)))
    (when session
      (if (haskell-session-cmd session)
          (haskell-session-collect session
                                   response
                                   session)
        (haskell-session-log
         (replace-regexp-in-string "\4" "" response))))))

(defun haskell-session-log (msg)
  "Effective append MSG to the process log (if enabled)."
  (when haskell-session-log
    (let* ((append-to (get-buffer-create "*haskell-session-log*"))
           (windows (get-buffer-window-list append-to t t))
           move-point-in-windows)
      (with-current-buffer append-to
        (setq buffer-read-only nil)
        ;; record in which windows we should keep point at eob.
        (dolist (window windows)
          (when (= (window-point window) (point-max))
            (push window move-point-in-windows)))
        (let (return-to-position)
          ;; decide whether we should reset point to return-to-position
          ;; or leave it at eob.
          (unless (= (point) (point-max))
            (setq return-to-position (point))
            (goto-char (point-max)))
          (insert "\n" msg "\n")
          (when return-to-position
          (goto-char return-to-position)))
        ;; advance to point-max in windows where it is needed
        (dolist (window move-point-in-windows)
          (set-window-point window (point-max)))
        (setq buffer-read-only t)))))

(defun haskell-session-project-by-proc (proc)
  "Find project by process."
  (cl-find-if (lambda (project)
                (string= (haskell-session-name project)
                         (process-name proc)))
              haskell-sessions))

(defun haskell-session-collect (_session response process)
  "Collect input for the response until receives a prompt."
  (haskell-session-set-response process
                                (concat (haskell-session-response process) response))
  (while (haskell-session-live-updates process))
  (when (string-match haskell-session-prompt-regex
                      (haskell-session-response process))
    (haskell-command-exec-complete
     (haskell-session-cmd process)
     (replace-regexp-in-string
      haskell-session-prompt-regex
      ""
      (haskell-session-response process)))
    (haskell-session-reset process)
    (haskell-session-trigger-queue process)))

(defun haskell-session-reset (process)
  "Reset the process's state, ready for the next send/reply."
  (progn (haskell-session-set-response-cursor process 0)
         (haskell-session-set-response process "")
         (haskell-session-set-cmd process nil)))

(defun haskell-session-consume (process regex)
  "Consume a regex from the response and move the cursor along if succeed."
  (when (string-match regex
                      (haskell-session-response process)
                      (haskell-session-response-cursor process))
    (haskell-session-set-response-cursor process (match-end 0))
    t))

(defun haskell-session-send-string (process string)
  "Try to send a string to the process's process. Ask to restart if it's not running."
  (let ((child (haskell-session-process process)))
    (if (equal 'run (process-status child))
        (let ((out (concat string "\n")))
          (haskell-session-log
           (propertize (concat (propertize "-> " 'face font-lock-comment-face)
                               (propertize string 'face font-lock-string-face))
                       'face '((:weight bold))))
          (process-send-string child out))
      (unless (haskell-session-restarting process)
        (run-hook-with-args 'haskell-session-ended process)))))

(defun haskell-session-live-updates (process)
  "Process live updates."
  (haskell-command-exec-live (haskell-session-cmd process)
                             (haskell-session-response process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making commands

(defun haskell-session-queue-without-filters (process line)
  "Queue LINE to be sent to PROCESS without bothering to look at
the response."
  (haskell-session-queue-command
   process
   (make-haskell-command
    :state (cons process line)
    :go (lambda (state)
          (haskell-session-send-string (car state)
                                       (cdr state))))))


(defun haskell-session-queue-command (process command)
  "Add a command to the process command queue."
  (haskell-session-cmd-queue-add process command)
  (haskell-session-trigger-queue process))

(defun haskell-session-trigger-queue (process)
  "Trigger the next command in the queue to be ran if there is no current command."
  (if (and (haskell-session-process process)
           (process-live-p (haskell-session-process process)))
      (unless (haskell-session-cmd process)
        (let ((cmd (haskell-session-cmd-queue-pop process)))
          (when cmd
            (haskell-session-set-cmd process cmd)
            (haskell-command-exec-go cmd))))
    (progn (haskell-session-reset process)
           (haskell-session-set process 'command-queue nil)
           (run-hook-with-args 'haskell-session-ended process))))

(defun haskell-session-queue-flushed-p (process)
  "Return t if command queue has been completely processed."
  (not (or (haskell-session-cmd-queue process)
           (haskell-session-cmd process))))

(defun haskell-session-queue-flush (process)
  "Block till PROCESS' command queue has been completely processed.
This uses `accept-process-output' internally."
  (while (not (haskell-session-queue-flushed-p process))
    (haskell-session-trigger-queue process)
    (accept-process-output (haskell-session-process process) 1)))

(defun haskell-session-queue-sync-request (process reqstr)
  "Queue submitting REQSTR to PROCESS and return response blockingly."
  (let ((cmd (make-haskell-command
              :state (cons nil process)
              :go `(lambda (s) (haskell-session-send-string (cdr s) ,reqstr))
              :complete 'setcar)))
    (haskell-session-queue-command process cmd)
    (haskell-session-queue-flush process)
    (car-safe (haskell-command-state cmd))))

(defun haskell-session-get-repl-completions (process inputstr &optional limit)
  "Perform `:complete repl ...' query for INPUTSTR using PROCESS.
Give optional LIMIT arg to limit completion candidates count,
zero, negative values, and nil means all possible completions.
Returns NIL when no completions found."
  (let* ((mlimit (if (and limit (> limit 0))
                     (concat " " (number-to-string limit) " ")
                   " "))
         (reqstr (concat ":complete repl"
                         mlimit
                         (haskell-string-literal-encode inputstr)))
         (rawstr (haskell-session-queue-sync-request process reqstr)))
    ;; TODO use haskell-utils-parse-repl-response
    (if (string-prefix-p "unknown command " rawstr)
        (error "GHCi lacks `:complete' support (try installing 7.8 or ghci-ng)")
      (let* ((s1 (split-string rawstr "\r?\n" t))
             (cs (mapcar #'haskell-string-literal-decode (cdr s1)))
             (h0 (car s1))) ;; "<limit count> <all count> <unused string>"
        (unless (string-match "\\`\\([0-9]+\\) \\([0-9]+\\) \\(\".*\"\\)\\'" h0)
          (error "Invalid `:complete' response"))
        (let ((cnt1 (match-string 1 h0))
              (h1 (haskell-string-literal-decode (match-string 3 h0))))
          (unless (= (string-to-number cnt1) (length cs))
            (error "Lengths inconsistent in `:complete' reponse"))
          (cons h1 cs))))))

;; Wrappers using haskell-session-{get,set}

(defun haskell-session-set-sent-stdin (p v)
  "We've sent stdin, so let's not clear the output at the end."
  (haskell-session-set p 'sent-stdin v))

(defun haskell-session-sent-stdin-p (p)
  "Did we send any stdin to the process during evaluation?"
  (haskell-session-get p 'sent-stdin))

(defun haskell-session-set-suggested-imports (p v)
  "Remember what imports have been suggested, to avoid
re-asking about the same imports."
  (haskell-session-set p 'suggested-imported v))

(defun haskell-session-suggested-imports (p)
  "Get what modules have already been suggested and accepted."
  (haskell-session-get p 'suggested-imported))

(defun haskell-session-set-evaluating (p v)
  "Set status of evaluating to be on/off."
  (haskell-session-set p 'evaluating v))

(defun haskell-session-evaluating-p (p)
  "Set status of evaluating to be on/off."
  (haskell-session-get p 'evaluating))

(defun haskell-session-set-process (p v)
  "Set the process's inferior process."
  (haskell-session-set p 'inferior-process v))

(defun haskell-session-process (p)
  "Get the process child."
  (haskell-session-get p 'inferior-process))

(defun haskell-session-name (p)
  "Get the process name."
  (haskell-session-get p 'name))

(defun haskell-session-cmd (p)
  "Get the process's current command.
Return nil if no current command."
  (haskell-session-get p 'current-command))

(defun haskell-session-set-cmd (p v)
  "Set the process's current command."
  (haskell-session-set-evaluating p nil)
  (haskell-session-set-sent-stdin p nil)
  (haskell-session-set-suggested-imports p nil)
  (haskell-session-set p 'current-command v))

(defun haskell-session-response (p)
  "Get the process's current response."
  (haskell-session-get p 'current-response))

(defun haskell-session-session (p)
  "Get the process's current session."
  (haskell-session-get p 'session))

(defun haskell-session-set-response (p v)
  "Set the process's current response."
  (haskell-session-set p 'current-response v))

(defun haskell-session-set-session (p v)
  "Set the process's current session."
  (haskell-session-set p 'session v))

(defun haskell-session-response-cursor (p)
  "Get the process's current response cursor."
  (haskell-session-get p 'current-response-cursor))

(defun haskell-session-set-response-cursor (p v)
  "Set the process's response cursor."
  (haskell-session-set p 'current-response-cursor v))

;; low-level command queue operations

(defun haskell-session-restarting (process)
  "Is the PROCESS restarting?"
  (haskell-session-get process 'is-restarting))

(defun haskell-session-cmd-queue (process)
  "Get the PROCESS' command queue.
New entries get added to the end of the list. Use
`haskell-session-cmd-queue-add' and
`haskell-session-cmd-queue-pop' to modify the command queue."
  (haskell-session-get process 'command-queue))

(defun haskell-session-cmd-queue-add (process cmd)
  "Add CMD to end of PROCESS's command queue."
  (cl-check-type cmd haskell-command)
  (haskell-session-set process
                       'command-queue
                       (append (haskell-session-cmd-queue process)
                               (list cmd))))

(defun haskell-session-cmd-queue-pop (process)
  "Pop the PROCESS' next entry from command queue.
Returns nil if queue is empty."
  (let ((queue (haskell-session-cmd-queue process)))
    (when queue
      (haskell-session-set process 'command-queue (cdr queue))
      (car queue))))


(defun haskell-session-unignore-file (session file)
  "

Note to Windows Emacs hackers:

chmod is how to change the mode of files in POSIX
systems. This will not work on your operating
system.

There is a command a bit like chmod called \"Calcs\"
that you can try using here:

http://technet.microsoft.com/en-us/library/bb490872.aspx

If it works, you can submit a patch to this
function and remove this comment.
"
  (shell-command (read-from-minibuffer "Permissions command: "
                                       (concat "chmod 700 "
                                               file)))
  (haskell-session-modify
   session
   'ignored-files
   (lambda (files)
     (cl-remove-if (lambda (path)
                     (string= path file))
                   files))))

(defun haskell-command-exec-go (command)
  "Call the command's go function."
  (let ((go-func (haskell-command-go command)))
    (when go-func
      (funcall go-func (haskell-command-state command)))))

(defun haskell-command-exec-complete (command response)
  "Call the command's complete function."
  (let ((comp-func (haskell-command-complete command)))
    (when comp-func
      (condition-case e
          (funcall comp-func
                   (haskell-command-state command)
                   response)
        (quit (message "Quit"))
        (error (message "Haskell process command errored with: %S" e))))))

(defun haskell-command-exec-live (command response)
  "Trigger the command's live updates callback."
  (let ((live-func (haskell-command-live command)))
    (when live-func
      (funcall live-func
               (haskell-command-state command)
               response))))

(provide 'haskell-process)

;;; haskell-session.el ends here
