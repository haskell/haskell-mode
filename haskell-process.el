;;; haskell-process.el -- Communicating with the inferior Haskell process.

;; Copyright (C) 2011-2012 Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

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

;;; Commentary:

;;; Todo:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

(defcustom haskell-process-bin
  ;; Arbitrarily give preference to hugs over ghci.
  (or (cond
       ((not (fboundp 'executable-find)) nil)
       ((executable-find "hugs") "hugs \"+.\"")
       ((executable-find "ghci") "ghci"))
      "hugs \"+.\"")
  "The name of the command to start the inferior Haskell process.
The command can include arguments."
  ;; Custom only supports the :options keyword for a few types, e.g. not
  ;; for string.
  ;; :options '("hugs \"+.\"" "ghci")
  :group 'haskell
  :type '(choice string (repeat string)))

(defvar haskell-process-prompt-regex "\\(^[> ]*> $\\|\n[> ]*> $\\)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialised commands

;;;###autoload
(defun haskell-process-load-file ()
  "Load the current buffer file."
  (interactive)
  (let ((file-path (buffer-file-name))
        (session (haskell-session))
        (process (haskell-process)))
    (haskell-process-queue-command
     process
     (haskell-command-make 
      (list session process file-path)
      (lambda (state)
        (haskell-process-send-string (cadr state)
                                     (format ":load %s" (caddr state))))
      nil
      (lambda (state response)
        (haskell-interactive-mode-echo (car state) response))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the process

(defun haskell-process-start (session)
  "Start the inferior Haskell process."
  (let ((process (haskell-process-make (haskell-session-name session))))
    (haskell-session-set-process session process)
    (haskell-process-set-session process session)
    (haskell-process-set-process
     process
     (start-process (haskell-session-name session)
                    nil
                    haskell-process-bin))
    (progn (set-process-sentinel (haskell-process-process process) 'haskell-process-sentinel)
           (set-process-filter (haskell-process-process process) 'haskell-process-filter))
    (haskell-process-send-startup process)
    process))

(defun haskell-process-restart ()
  "Restart the inferior Haskell process."
  (interactive)
  (haskell-process-start (haskell-session)))

(defun haskell-process-make (name)
  "Make an inferior Haskell process."
  (list (cons 'name name)
        (cons 'current-command
              (haskell-command-make nil nil nil nil))))

(defun haskell-process ()
  "Get the current process from the current session."
  (haskell-session-process (haskell-session)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun haskell-process-send-startup (process)
  "Send the necessary start messages."
  (haskell-process-queue-command
   process
   (haskell-command-make process
                         (lambda (process) (haskell-process-send-string process ":set prompt \"> \"\n"))
                         nil
                         nil))
  (haskell-process-queue-command
   process
   (haskell-command-make process
                         (lambda (process) (haskell-process-send-string process ":set -v1\n"))
                         nil
                         (lambda (process _)
                           (haskell-interactive-mode-echo
                            (haskell-process-session process)
                            (nth (random (length haskell-interactive-greetings))
                                 haskell-interactive-greetings))))))

(defun haskell-process-sentinel (proc event)
  "The sentinel for the process pipe."
  (let ((session (haskell-process-project-by-proc proc)))
    (when session
      (let ((process (haskell-session-process session)))
        (haskell-process-reset process)
        (haskell-process-log (format "Event: %S\n" event))
        (haskell-process-log "Process reset.")
        (haskell-process-prompt-restart process)))))

(defun haskell-process-filter (proc response)
  "The filter for the process pipe."
  (haskell-process-log (format "<- %S\n" response))
  (let ((session (haskell-process-project-by-proc proc)))
    (when session
      (when (not (eq (haskell-process-cmd (haskell-session-process session))
                     'none))
        (haskell-process-collect session
                                 response
                                 (haskell-session-process session)
                                 'main)))))

(defun haskell-process-log (out)
  "Log to the process log."
  (with-current-buffer (get-buffer-create "*haskell-process-log*")
    (insert out)
    (goto-char (point-max))))

(defun haskell-process-project-by-proc (proc)
  "Find project by process."
  (find-if (lambda (project)
             (string= (haskell-session-name project)
                      (process-name proc)))
           haskell-sessions))

(defun haskell-process-collect (session response process type)
  "Collect input for the response until receives a prompt."
  (haskell-process-set-response process
                                (concat (haskell-process-response process) response))
  (while (haskell-process-live-updates session process))
  (when (string-match haskell-process-prompt-regex
                      (haskell-process-response process))
    (haskell-command-complete
     (haskell-process-cmd process)
     (replace-regexp-in-string
      haskell-process-prompt-regex
      ""
      (haskell-process-response process)))
    (haskell-process-reset process)
    (haskell-process-trigger-queue process)))

(defun haskell-process-reset (process)
  "Reset the process's state, ready for the next send/reply."
  (progn (haskell-process-set-response-cursor process 0)
         (haskell-process-set-response process "")
         (haskell-process-set-cmd process 'none)))

(defun haskell-process-send-string (process string)
  "Try to send a string to the process's process. Ask to restart if it's not running."
  (let ((child (haskell-process-process process)))
    (if (equal 'run (process-status child))
        (let ((out (concat string "\n")))
          (haskell-process-log (format "-> %S\n" out))
          (process-send-string child out))
      (haskell-process-prompt-restart process))))

(defun haskell-process-prompt-restart (process)
  "Prompt to restart the died process."
  (when (y-or-n-p (format "The Haskell process `%s' has died. Restart? "
                          (haskell-process-name process)))
    (haskell-process-start (haskell-process-session process))))

(defun haskell-process-live-updates (project process)
  "Process live updates."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making commands

(defun haskell-process-queue-command (process command)
  "Add a command to the process command queue."
  (haskell-process-add-to-cmd-queue process command)
  (haskell-process-trigger-queue process))

(defun haskell-process-trigger-queue (process)
  "Trigger the next command in the queue to be ran if there is no current command."
  (when (equal (haskell-process-cmd process) 'none)
    (let ((cmd (haskell-process-cmd-queue-pop process)))
      (when cmd
        (haskell-process-set-cmd process cmd)
        (haskell-command-go cmd)))))

(defun haskell-command-make (state go live complete)
  "Make a process command of the given `type' with the given `go' procedure."
  (list (cons 'state state)
        (cons 'go go)
        (cons 'live live)
        (cons 'complete complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing the process

(defun haskell-process-set-process (p v)
  "Set the process's inferior process."
  (haskell-process-set p 'inferior-process v))

(defun haskell-process-process (p)
  "Get the process child."
  (haskell-process-get p 'inferior-process))

(defun haskell-process-name (p)
  "Get the process name."
  (haskell-process-get p 'name))

(defun haskell-process-cmd (p)
  "Get the process's current command."
  (haskell-process-get p 'current-command))

(defun haskell-process-set-cmd (p v)
  "Set the process's current command."
  (haskell-process-set p 'current-command v))

(defun haskell-process-response (p)
  "Get the process's current response."
  (haskell-process-get p 'current-response))

(defun haskell-process-session (p)
  "Get the process's current session."
  (haskell-process-get p 'session))

(defun haskell-process-set-response (p v)
  "Set the process's current response."
  (haskell-process-set p 'current-response v))

(defun haskell-process-set-session (p v)
  "Set the process's current session."
  (haskell-process-set p 'session v))

(defun haskell-process-response-cursor (p)
  "Get the process's current response cursor."
  (haskell-process-get p 'current-response-cursor))

(defun haskell-process-set-response-cursor (p v)
  "Set the process's response cursor."
  (haskell-process-set p 'current-response-cursor v))

(defun haskell-process-add-to-cmd-queue (process cmd)
  "Set the process's response cursor."
  (haskell-process-set process
                       'command-queue
                       (append (haskell-process-cmd-queue process)
                               (list cmd))))

(defun haskell-process-cmd-queue (process)
  "Get the process's command queue."
  (haskell-process-get process 'command-queue))

(defun haskell-process-cmd-queue-pop (process)
  "Get the process's command queue."
  (let ((queue (haskell-process-get process 'command-queue)))
    (unless (null queue)
      (let ((next (car queue)))
        (haskell-process-set process 'command-queue (cdr queue))
        next))))

(defun haskell-process-get (s key)
  "Get the process `key'."
  (let ((x (assoc key s)))
    (when x
      (cdr x))))

(defun haskell-process-set (s key value) 
  "Set the process's `key'."
  (delete-if (lambda (prop) (equal (car prop) key)) s)
  (setf (cdr s) (cons (cons key value)
                      (cdr s)))
  s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing commands

(defun haskell-command-type (s)
  "Get the command's type."
  (haskell-command-get s 'type))

(defun haskell-command-state (s)
  "Get the command's state."
  (haskell-command-get s 'state))

(defun haskell-command-go (s)
  "Call the command's go function."
  (let ((func (haskell-command-get s 'go)))
    (when func
      (funcall func 
               (haskell-command-state s)))))

(defun haskell-command-complete (s response)
  "Call the command's complete function."
  (let ((func (haskell-command-get s 'complete)))
    (when func
      (funcall func
               (haskell-command-state s)
               response))))

(defun haskell-command-get (s key)
  "Get the command `key'."
  (let ((x (assoc key s)))
    (when x
      (cdr x))))

(defun haskell-command-set (s key value) 
  "Set the command's `key'."
  (delete-if (lambda (prop) (equal (car prop) key)) s)
  (setf (cdr s) (cons (cons key value)
                      (cdr s)))
  s)
