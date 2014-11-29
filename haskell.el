;;; haskell.el --- Top-level Haskell package

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'haskell-process)
(require 'haskell-debug)
(require 'haskell-interactive-mode)
(require 'haskell-repl)
(require 'haskell-presentation-mode)
(require 'haskell-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic configuration hooks

(add-hook 'haskell-process-ended-hook 'haskell-process-prompt-restart)
(add-hook 'kill-buffer-hook 'haskell-interactive-kill)

(defun haskell-interactive-mode-return ()
  "Handle the return key."
  (interactive)
  (cond
   ((haskell-interactive-at-compile-message)
    (next-error-internal))
   (t
    (haskell-interactive-handle-expr))))

(defun haskell-session-kill (&optional leave-interactive-buffer)
  "Kill the session process and buffer, delete the session.
0. Prompt to kill all associated buffers.
1. Kill the process.
2. Kill the interactive buffer.
3. Walk through all the related buffers and set their haskell-session to nil.
4. Remove the session from the sessions list."
  (interactive)
  (let* ((session (haskell-session))
         (name (haskell-session-name session))
         (also-kill-buffers
          (and haskell-ask-also-kill-buffers
               (y-or-n-p (format "Killing `%s'. Also kill all associated buffers?" name)))))
    (haskell-kill-session-process session)
    (unless leave-interactive-buffer
      (kill-buffer (haskell-session-interactive-buffer session)))
    (cl-loop for buffer in (buffer-list)
             do (with-current-buffer buffer
                  (when (and (boundp 'haskell-session)
                             (string= (haskell-session-name haskell-session) name))
                    (setq haskell-session nil)
                    (when also-kill-buffers
                      (kill-buffer)))))
    (setq haskell-sessions
          (cl-remove-if (lambda (session)
                          (string= (haskell-session-name session)
                                   name))
                        haskell-sessions))))

(defun haskell-interactive-kill ()
  "Kill the buffer and (maybe) the session."
  (interactive)
  (when (eq major-mode 'haskell-interactive-mode)
    (when (and (boundp 'haskell-session)
               haskell-session
               (y-or-n-p "Kill the whole session?"))
      (haskell-session-kill t))))

(defun haskell-session-make (name)
  "Make a Haskell session."
  (when (haskell-session-lookup name)
    (error "Session of name %s already exists!" name))
  (let ((session (setq haskell-session
                       (list (cons 'name name)))))
    (add-to-list 'haskell-sessions session)
    (haskell-process-start session)
    session))

(defun haskell-session-new-assume-from-cabal ()
  "Prompt to create a new project based on a guess from the nearest Cabal file."
  (let ((name (haskell-session-default-name)))
    (unless (haskell-session-lookup name)
      (when (y-or-n-p (format "Start a new project named “%s”? "
                              name))
        (haskell-session-make name)))))

;;;###autoload
(defun haskell-session ()
  "Get the Haskell session, prompt if there isn't one or fail."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-new-assume-from-cabal)
           (haskell-session-choose)
           (haskell-session-new)))))

;;;###autoload
(defun haskell-interactive-bring ()
  "Bring up the interactive mode for this session."
  (interactive)
  (let* ((session (haskell-session))
         (buffer (haskell-session-interactive-buffer session)))
    (unless (and (cl-find-if (lambda (window) (equal (window-buffer window) buffer))
                             (window-list))
                 (= 2 (length (window-list))))
      (delete-other-windows)
      (display-buffer buffer)
      (other-window 1))))

;;;###autoload
(defun haskell-interactive-switch ()
  "Switch to the interactive mode for this session."
  (interactive)
  (let ((initial-buffer (current-buffer))
        (buffer (haskell-session-interactive-buffer (haskell-session))))
    (with-current-buffer buffer
      (setq haskell-interactive-previous-buffer initial-buffer))
    (unless (eq buffer (window-buffer))
      (switch-to-buffer-other-window buffer))))

(defun haskell-session-new ()
  "Make a new session."
  (let ((name (read-from-minibuffer "Project name: " (haskell-session-default-name))))
    (when (not (string= name ""))
      (let ((session (haskell-session-lookup name)))
        (if session
            (when (y-or-n-p (format "Session %s already exists. Use it?" name))
              session)
          (haskell-session-make name))))))

;;;###autoload
(defun haskell-session-change ()
  "Change the session for the current buffer."
  (interactive)
  (haskell-session-assign (or (haskell-session-new-assume-from-cabal)
                              (haskell-session-choose)
                              (haskell-session-new))))

;;;###autoload
(defun haskell-mode-find-def (ident)
  "Find definition location of identifier. Uses the GHCi process
to find the location.

Returns:

    (library <package> <module>)
    (file <path> <line> <col>)
    (module <name>)
"
  (let ((reply (haskell-process-queue-sync-request
                (haskell-process)
                (format (if (string-match "^[a-zA-Z_]" ident)
                            ":info %s"
                          ":info (%s)")
                        ident))))
    (let ((match (string-match "-- Defined \\(at\\|in\\) \\(.+\\)$" reply)))
      (when match
        (let ((defined (match-string 2 reply)))
          (let ((match (string-match "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)$" defined)))
            (cond
             (match
              (list 'file
                    (expand-file-name (match-string 1 defined)
                                      (haskell-session-current-dir (haskell-session)))
                    (string-to-number (match-string 2 defined))
                    (string-to-number (match-string 3 defined))))
             (t
              (let ((match (string-match "`\\(.+?\\):\\(.+?\\)'$" defined)))
                (if match
                    (list 'library
                          (match-string 1 defined)
                          (match-string 2 defined))
                  (let ((match (string-match "`\\(.+?\\)'$" defined)))
                    (if match
                        (list 'module
                              (match-string 1 defined))))))))))))))

;;;###autoload
(defun haskell-mode-jump-to-def (ident)
  "Jump to definition of identifier at point."
  (interactive (list (haskell-ident-at-point)))
  (let ((loc (haskell-mode-find-def ident)))
    (when loc
      (haskell-mode-handle-generic-loc loc))))

;;;###autoload
(defun haskell-process-minimal-imports ()
  "Dump minimal imports."
  (interactive)
  (unless (> (save-excursion
               (goto-char (point-min))
               (haskell-navigate-imports-go)
               (point))
             (point))
    (goto-char (point-min))
    (haskell-navigate-imports-go))
  (haskell-process-queue-sync-request (haskell-process)
                                      ":set -ddump-minimal-imports")
  (haskell-process-load-file)
  (insert-file-contents-literally
   (concat (haskell-session-current-dir (haskell-session))
           "/"
           (haskell-guess-module-name)
           ".imports")))

;;;###autoload
(defun haskell-mode-jump-to-def-or-tag (&optional next-p)
  "Jump to the definition (by consulting GHCi), or (fallback)
jump to the tag.

Remember: If GHCi is busy doing something, this will delay, but
it will always be accurate, in contrast to tags, which always
work but are not always accurate.

If the definition or tag is found, the location from which you
jumped will be pushed onto `find-tag-marker-ring', so you can
return to that position with `pop-tag-mark'."
  (interactive "P")
  (let ((initial-loc (point-marker))
        (loc (haskell-mode-find-def (haskell-ident-at-point))))
    (if loc
        (haskell-mode-handle-generic-loc loc)
      (call-interactively 'haskell-mode-tag-find))
    (unless (equal initial-loc (point-marker))
      ;; Store position for return with `pop-tag-mark'
      (ring-insert find-tag-marker-ring initial-loc))))

;;;###autoload
(defun haskell-mode-goto-loc ()
  "Go to the location of the thing at point. Requires the :loc-at
command from GHCi."
  (interactive)
  (let ((loc (haskell-mode-loc-at)))
    (when loc
      (find-file (expand-file-name (plist-get loc :path)
                                   (haskell-session-cabal-dir (haskell-session))))
      (goto-char (point-min))
      (forward-line (1- (plist-get loc :start-line)))
      (forward-char (plist-get loc :start-col)))))

;;;###autoload
(defun haskell-process-load-file ()
  "Load the current buffer file."
  (interactive)
  (save-buffer)
  (haskell-interactive-mode-reset-error (haskell-session))
  (haskell-process-file-loadish (format "load \"%s\"" (replace-regexp-in-string
                                                       "\""
                                                       "\\\\\""
                                                       (buffer-file-name)))
                                nil
                                (current-buffer)))

;;;###autoload
(defun haskell-process-reload-file ()
  "Re-load the current buffer file."
  (interactive)
  (save-buffer)
  (haskell-interactive-mode-reset-error (haskell-session))
  (haskell-process-file-loadish "reload" t nil))

;;;###autoload
(defun haskell-process-load-or-reload (&optional toggle)
  "Load or reload. Universal argument toggles which."
  (interactive "P")
  (if toggle
      (progn (setq haskell-reload-p (not haskell-reload-p))
             (message "%s (No action taken this time)"
                      (if haskell-reload-p
                          "Now running :reload."
                        "Now running :load <buffer-filename>.")))
    (if haskell-reload-p (haskell-process-reload-file) (haskell-process-load-file))))

;;;###autoload
(defun haskell-process-file-loadish (command reload-p module-buffer)
  "Run a loading-ish COMMAND that wants to pick up type errors
and things like that. RELOAD-P indicates whether the notification
should say 'reloaded' or 'loaded'. MODULE-BUFFER may be used
for various things, but is optional."
  (let ((session (haskell-session)))
    (haskell-session-current-dir session)
    (when haskell-process-check-cabal-config-on-load
      (haskell-process-look-config-changes session))
    (let ((process (haskell-process)))
      (haskell-process-queue-command
       process
       (make-haskell-command
        :state (list session process command reload-p module-buffer)
        :go (lambda (state)
              (haskell-process-send-string
               (cadr state) (format ":%s" (cl-caddr state))))
        :live (lambda (state buffer)
                (haskell-process-live-build
                 (cadr state) buffer nil))
        :complete (lambda (state response)
                    (haskell-process-load-complete
                     (car state)
                     (cadr state)
                     response
                     (cl-cadddr state)
                     (cl-cadddr (cdr state)))))))))

;;;###autoload
(defun haskell-process-cabal-build ()
  "Build the Cabal project."
  (interactive)
  (haskell-process-do-cabal "build")
  (haskell-process-add-cabal-autogen))

;;;###autoload
(defun haskell-process-cabal (p)
  "Prompts for a Cabal command to run."
  (interactive "P")
  (if p
      (haskell-process-do-cabal
       (read-from-minibuffer "Cabal command (e.g. install): "))
    (haskell-process-do-cabal
     (funcall haskell-completing-read-function "Cabal command: "
              (append haskell-cabal-commands
                      (list "build --ghc-options=-fforce-recomp"))))))



(defun haskell-process-add-cabal-autogen ()
  "Add <cabal-project-dir>/dist/build/autogen/ to the ghci search
path. This allows modules such as 'Path_...', generated by cabal,
to be loaded by ghci."
  (unless (eq 'cabal-repl (haskell-process-type)) ;; redundant with "cabal repl"
    (let*
        ((session       (haskell-session))
         (cabal-dir     (haskell-session-cabal-dir session))
         (ghci-gen-dir  (format "%sdist/build/autogen/" cabal-dir)))
      (haskell-process-queue-without-filters
       (haskell-process)
       (format ":set -i%s" ghci-gen-dir)))))

(defun haskell-process-restart ()
  "Restart the inferior Haskell process."
  (interactive)
  (haskell-process-reset (haskell-process))
  (haskell-process-set (haskell-process) 'command-queue nil)
  (haskell-process-start (haskell-session)))

(defun haskell-process-unignore ()
  "Unignore any files that were specified as being ignored by the
  inferior GHCi process."
  (interactive)
  (let ((session (haskell-session))
        (changed nil))
    (if (null (haskell-session-get session
                                   'ignored-files))
        (message "Nothing to unignore!")
      (cl-loop for file in (haskell-session-get session
                                                'ignored-files)
               do (cl-case (read-event
                            (propertize (format "Set permissions? %s (y, n, v: stop and view file)"
                                                file)
                                        'face 'minibuffer-prompt))
                    (?y
                     (haskell-process-unignore-file session file)
                     (setq changed t))
                    (?v
                     (find-file file)
                     (cl-return))))
      (when (and changed
                 (y-or-n-p "Restart GHCi process now? "))
        (haskell-process-restart)))))

(defun haskell-process-reload-devel-main ()
  "Reload the module `DevelMain' and then run
`DevelMain.update'. This is for doing live update of the code of
servers or GUI applications. Put your development version of the
program in `DevelMain', and define `update' to auto-start the
program on a new thread, and use the `foreign-store' package to
access the running context across :load/:reloads in GHCi."
  (interactive)
  (with-current-buffer (or (get-buffer "DevelMain.hs")
                           (if (y-or-n-p "You need to open a buffer named DevelMain.hs. Find now?")
                               (ido-find-file)
                             (error "No DevelMain.hs buffer.")))
    (let ((session (haskell-session)))
      (let ((process (haskell-process)))
        (haskell-process-queue-command
         process
         (make-haskell-command
          :state (list :session session
                       :process process
                       :buffer (current-buffer))
          :go (lambda (state)
                (haskell-process-send-string (plist-get state ':process)
                                             ":l DevelMain"))
          :live (lambda (state buffer)
                  (haskell-process-live-build (plist-get state ':process)
                                              buffer
                                              nil))
          :complete (lambda (state response)
                      (haskell-process-load-complete
                       (plist-get state ':session)
                       (plist-get state ':process)
                       response
                       nil
                       (plist-get state ':buffer)
                       (lambda (ok)
                         (when ok
                           (haskell-process-queue-without-filters
                            (haskell-process)
                            "DevelMain.update")
                           (message "DevelMain updated.")))))))))))

(defun haskell-process-generate-tags (&optional and-then-find-this-tag)
  "Regenerate the TAGS table."
  (interactive)
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process and-then-find-this-tag)
      :go (lambda (state)
            (if (eq system-type 'windows-nt)
                (haskell-process-send-string
                 (car state)
                 (format ":!powershell -Command \"& { cd %s ; hasktags -e -x (ls -fi *.hs -exclude \\\"#*#\\\" -name -r) ; exit }\""
                         (haskell-session-cabal-dir
                          (haskell-process-session (car state)))))
              (haskell-process-send-string
               (car state)
               (format ":!cd %s && %s | %s | %s"
                       (haskell-session-cabal-dir
                        (haskell-process-session (car state)))
                       "find . -name '*.hs*'"
                       "grep -v '#'" ; To avoid Emacs back-up files. Yeah.
                       "xargs hasktags -e -x"))))
      :complete (lambda (state response)
                  (when (cdr state)
                    (let ((tags-file-name
                           (haskell-session-tags-filename
                            (haskell-process-session (car state)))))
                      (find-tag (cdr state))))
                  (haskell-mode-message-line "Tags generated."))))))

(defun haskell-process-insert-type ()
  "Get the identifer at the point and insert its type, if
possible, using GHCi's :type."
  (let ((process (haskell-process))
        (query (let ((ident (haskell-ident-at-point)))
                 (format (if (string-match "^[_[:lower:][:upper:]]" ident)
                             ":type %s"
                           ":type (%s)")
                         ident))))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list process query (current-buffer))
      :go (lambda (state)
            (haskell-process-send-string (nth 0 state)
                                         (nth 1 state)))
      :complete (lambda (state response)
                  (cond
                   ;; TODO: Generalize this into a function.
                   ((or (string-match "^Top level" response)
                        (string-match "^<interactive>" response))
                    (message response))
                   (t
                    (with-current-buffer (nth 2 state)
                      (goto-char (line-beginning-position))
                      (insert (format "%s\n" (replace-regexp-in-string "\n$" "" response)))))))))))

(defun haskell-process-do-try-info (sym)
  "Get info of `sym' and echo in the minibuffer."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process sym)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (if (string-match "^[A-Za-z_]" (cdr state))
                 (format ":info %s" (cdr state))
               (format ":info (%s)" (cdr state)))))
      :complete (lambda (state response)
                  (unless (or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                    (haskell-mode-message-line response)))))))

(defun haskell-process-do-try-type (sym)
  "Get type of `sym' and echo in the minibuffer."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process sym)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (if (string-match "^[A-Za-z_]" (cdr state))
                 (format ":type %s" (cdr state))
               (format ":type (%s)" (cdr state)))))
      :complete (lambda (state response)
                  (unless (or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                    (haskell-mode-message-line response)))))))

(defun haskell-mode-loc-at ()
  "Get the location at point. Requires the :loc-at command from
GHCi."
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (haskell-ident-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (let ((reply (haskell-process-queue-sync-request
                    (haskell-process)
                    (save-excursion
                      (format ":loc-at %s %d %d %d %d %s"
                              (buffer-file-name)
                              (progn (goto-char (car pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (progn (goto-char (cdr pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (buffer-substring-no-properties (car pos)
                                                              (cdr pos)))))))
        (if reply
            (if (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                              reply)
                (list :path (match-string 1 reply)
                      :start-line (string-to-number (match-string 2 reply))
                      ;; ;; GHC uses 1-based columns.
                      :start-col (1- (string-to-number (match-string 3 reply)))
                      :end-line (string-to-number (match-string 4 reply))
                      ;; GHC uses 1-based columns.
                      :end-col (1- (string-to-number (match-string 5 reply))))
              (error (propertize reply 'face 'compilation-error)))
          (error (propertize "No reply. Is :loc-at supported?"
                             'face 'compilation-error)))))))

(defun haskell-mode-type-at ()
  "Get the type of the thing at point. Requires the :type-at
command from GHCi."
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (haskell-ident-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (replace-regexp-in-string
       "\n$"
       ""
       (save-excursion
         (haskell-process-queue-sync-request
          (haskell-process)
          (replace-regexp-in-string
           "\n"
           " "
           (format ":type-at %s %d %d %d %d %s"
                   (buffer-file-name)
                   (progn (goto-char (car pos))
                          (line-number-at-pos))
                   (1+ (current-column))
                   (progn (goto-char (cdr pos))
                          (line-number-at-pos))
                   (1+ (current-column))
                   (buffer-substring-no-properties (car pos)
                                                   (cdr pos))))))))))

(defun haskell-process-cd (&optional not-interactive)
  "Change directory."
  (interactive)
  (let* ((session (haskell-session))
         (dir (haskell-session-pwd session t)))
    (haskell-process-log
     (propertize (format "Changing directory to %s ...\n" dir)
                 'face font-lock-comment-face))
    (haskell-process-change-dir session
                                (haskell-process)
                                dir)))

(defun haskell-session-pwd (session &optional change)
  "Prompt for the current directory."
  (or (unless change
        (haskell-session-get session 'current-dir))
      (progn (haskell-session-set-current-dir
              session
              (haskell-utils-read-directory-name
               (if change "Change directory: " "Set current directory: ")
               (or (haskell-session-get session 'current-dir)
                   (haskell-session-get session 'cabal-dir)
                   (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     "~/"))))
             (haskell-session-get session 'current-dir))))

(defun haskell-process-cabal-macros ()
  "Send the cabal macros string."
  (interactive)
  (haskell-process-queue-without-filters (haskell-process)
                                         ":set -optP-include -optPdist/build/autogen/cabal_macros.h"))

(defun haskell-mode-show-type-at (&optional insert-value)
  "Show the type of the thing at point."
  (interactive "P")
  (let ((ty (haskell-mode-type-at)))
    (if insert-value
        (progn (goto-char (line-beginning-position))
               (insert (haskell-fontify-as-mode ty 'haskell-mode)
                       "\n"))
      (message "%s" (haskell-fontify-as-mode ty 'haskell-mode)))))

(defun haskell-mode-handle-generic-loc (loc)
  "Either jump to or display a generic location. Either a file or
a library."
  (cl-case (car loc)
    (file (haskell-mode-jump-to-loc (cdr loc)))
    (library (message "Defined in `%s' (%s)."
                      (elt loc 2)
                      (elt loc 1)))
    (module (message "Defined in `%s'."
                     (elt loc 1)))))

(defun haskell-process-prompt-restart (process)
  "Prompt to restart the died process."
  (let ((process-name (haskell-process-name process)))
    (if haskell-process-suggest-restart
        (cl-case (read-event
                  (propertize (format "The Haskell process `%s' has died. Restart? (y, n, l: show process log)"
                                      process-name)
                              'face 'minibuffer-prompt))
          (?y (haskell-process-start (haskell-process-session process)))
          (?l (let* ((response (haskell-process-response process))
                     (buffer (get-buffer "*haskell-process-log*")))
                (if buffer
                    (switch-to-buffer buffer)
                  (progn (switch-to-buffer (get-buffer-create "*haskell-process-log*"))
                         (insert response)))))
          (?n))
      (message (format "The Haskell process `%s' is dearly departed."
                       process-name)))))

(defun haskell-session-change-target (target)
  "Set the build target for cabal repl"
  (interactive "sNew build target:")
  (let* ((session haskell-session)
         (old-target (haskell-session-get session 'target)))
    (when session
      (haskell-session-set-target session target)
      (when (and (not (string= old-target target))
                 (y-or-n-p "Target changed, restart haskell process?"))
        (haskell-process-start session)))))

(defun haskell-process-live-build (process buffer echo-in-repl)
  "Show live updates for loading files."
  (cond ((haskell-process-consume
          process
          (concat "\\[[ ]*\\([0-9]+\\) of \\([0-9]+\\)\\]"
                  " Compiling \\([^ ]+\\)[ ]+"
                  "( \\([^ ]+\\), \\([^ ]+\\) )[^\r\n]*[\r\n]+"))
         (haskell-process-echo-load-message process buffer echo-in-repl nil)
         t)
        ((haskell-process-consume
          process
          (concat "\\[[ ]*\\([0-9]+\\) of \\([0-9]+\\)\\]"
                  " Compiling \\[TH\\] \\([^ ]+\\)[ ]+"
                  "( \\([^ ]+\\), \\([^ ]+\\) )[^\r\n]*[\r\n]+"))
         (haskell-process-echo-load-message process buffer echo-in-repl t)
         t)
        ((haskell-process-consume process "Loading package \\([^ ]+\\) ... linking ... done.\n")
         (haskell-mode-message-line
          (format "Loading: %s"
                  (match-string 1 buffer)))
         t)
        ((haskell-process-consume
          process
          "^Preprocessing executables for \\(.+?\\)\\.\\.\\.")
         (let ((msg (format "Preprocessing: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo
            (haskell-process-session process)
            msg)
           (haskell-mode-message-line msg)))
        ((haskell-process-consume process "Linking \\(.+?\\) \\.\\.\\.")
         (let ((msg (format "Linking: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo (haskell-process-session process) msg)
           (haskell-mode-message-line msg)))
        ((haskell-process-consume process "\nBuilding \\(.+?\\)\\.\\.\\.")
         (let ((msg (format "Building: %s" (match-string 1 buffer))))
           (haskell-interactive-mode-echo
            (haskell-process-session process)
            msg)
           (haskell-mode-message-line msg)))))

(defun haskell-process-echo-load-message (process buffer echo-in-repl th)
  "Echo a load message."
  (let ((session (haskell-process-session process))
        (module-name (match-string 3 buffer))
        (file-name (match-string 4 buffer)))
    (haskell-interactive-show-load-message
     session
     'compiling
     module-name
     (haskell-session-strip-dir session file-name)
     echo-in-repl
     th)))

(defun haskell-process-change-dir (session process dir)
  "Change the directory of the current process."
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state (list session process dir)

    :go
    (lambda (state)
      (haskell-process-send-string
       (cadr state) (format ":cd %s" (cl-caddr state))))

    :complete
    (lambda (state _)
      (haskell-session-set-current-dir (car state) (cl-caddr state))
      (haskell-interactive-mode-echo (car state)
                                     (format "Changed directory: %s"
                                             (cl-caddr state)))))))

(defun haskell-process-send-startup (process)
  "Send the necessary start messages."
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state process

    :go (lambda (process)
          (haskell-process-send-string process ":set prompt \"\\4\"")
          (haskell-process-send-string process "Prelude.putStrLn \"\"")
          (haskell-process-send-string process ":set -v1"))

    :live (lambda (process buffer)
            (when (haskell-process-consume
                   process
                   "^\*\*\* WARNING: \\(.+\\) is writable by someone else, IGNORING!$")
              (let ((path (match-string 1 buffer)))
                (haskell-session-modify
                 (haskell-process-session process)
                 'ignored-files
                 (lambda (files)
                   (cl-remove-duplicates (cons path files) :test 'string=)))
                (haskell-interactive-mode-compile-warning
                 (haskell-process-session process)
                 (format "GHCi is ignoring: %s (run M-x haskell-process-unignore)"
                         path)))))

    :complete (lambda (process _)
                (haskell-interactive-mode-echo
                 (haskell-process-session process)
                 (concat (nth (random (length haskell-process-greetings))
                              haskell-process-greetings)
                         (when haskell-process-show-debug-tips
                           "
If I break, you can:
  1. Restart:           M-x haskell-process-restart
  2. Configure logging: C-h v haskell-process-log (useful for debugging)
  3. General config:    M-x customize-mode
  4. Hide these tips:   C-h v haskell-process-show-debug-tips")))))))

(defun haskell-process-errors-warnings (session process buffer)
  "Trigger handling type errors or warnings."
  (cond
   ((haskell-process-consume
     process
     "\\(Module imports form a cycle:[ \n]+module [^ ]+ ([^)]+)[[:unibyte:][:nonascii:]]+?\\)\nFailed")
    (let ((err (match-string 1 buffer)))
      (when (string-match "module [`'‘‛]\\([^ ]+\\)['’`] (\\([^)]+\\))" err)
        (let* ((default-directory (haskell-session-current-dir session))
               (module (match-string 1 err))
               (file (match-string 2 err))
               (relative-file-name (file-relative-name file)))
          (haskell-interactive-show-load-message
           session
           'import-cycle
           module
           relative-file-name
           nil
           nil)
          (haskell-interactive-mode-compile-error
           session
           (format "%s:1:0: %s"
                   relative-file-name
                   err)))))
    t)
   ((haskell-process-consume
     process
     (concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
             "[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]"))
    (haskell-process-set-response-cursor process
                                         (- (haskell-process-response-cursor process) 1))
    (let* ((buffer (haskell-process-response process))
           (file (match-string 1 buffer))
           (location (match-string 2 buffer))
           (error-msg (match-string 3 buffer))
           (warning (string-match "^Warning:" error-msg))
           (splice (string-match "^Splicing " error-msg))
           (final-msg (format "%s:%s: %s"
                              (haskell-session-strip-dir session file)
                              location
                              error-msg)))
      (funcall (cond (warning
                      'haskell-interactive-mode-compile-warning)
                     (splice
                      'haskell-interactive-mode-compile-splice)
                     (t 'haskell-interactive-mode-compile-error))
               session final-msg)
      (unless warning
        (haskell-mode-message-line final-msg))
      (haskell-process-trigger-suggestions
       session
       error-msg
       file
       (plist-get (haskell-process-parse-error final-msg) :line)))
    t)))

(defun haskell-process-load-complete (session process buffer reload module-buffer &optional cont)
  "Handle the complete loading response. BUFFER is the string of
text being sent over the process pipe. MODULE-BUFFER is the
actual Emacs buffer of the module being loaded."
  (cond ((haskell-process-consume process "Ok, modules loaded: \\(.+\\)\\.$")
         (let* ((modules (haskell-process-extract-modules buffer))
                (cursor (haskell-process-response-cursor process)))
           (haskell-process-set-response-cursor process 0)
           (let ((warning-count 0))
             (while (haskell-process-errors-warnings session process buffer)
               (setq warning-count (1+ warning-count)))
             (haskell-process-set-response-cursor process cursor)
             (if (and (not reload)
                      haskell-process-reload-with-fbytecode)
                 (haskell-process-reload-with-fbytecode process module-buffer)
               (haskell-process-import-modules process (car modules)))
             (haskell-mode-message-line
              (if reload "Reloaded OK." "OK."))
             (when cont
               (condition-case e
                   (funcall cont t)
                 (error (message "%S" e))
                 (quit nil))))))
        ((haskell-process-consume process "Failed, modules loaded: \\(.+\\)\\.$")
         (let* ((modules (haskell-process-extract-modules buffer))
                (cursor (haskell-process-response-cursor process)))
           (haskell-process-set-response-cursor process 0)
           (while (haskell-process-errors-warnings session process buffer))
           (haskell-process-set-response-cursor process cursor)
           (if (and (not reload) haskell-process-reload-with-fbytecode)
               (haskell-process-reload-with-fbytecode process module-buffer)
             (haskell-process-import-modules process (car modules)))
           (haskell-interactive-mode-compile-error session "Compilation failed.")
           (when cont
             (condition-case e
                 (funcall cont nil)
               (error (message "%S" e))
               (quit nil)))))))

(defun haskell-process-suggest-imports (session file modules ident)
  "Given a list of MODULES, suggest adding them to the import section."
  (cl-assert session)
  (cl-assert file)
  (cl-assert ident)
  (let* ((process (haskell-session-process session))
         (suggested-already (haskell-process-suggested-imports process))
         (module (cond ((> (length modules) 1)
                        (when (y-or-n-p (format "Identifier `%s' not in scope, choose module to import?"
                                                ident))
                          (haskell-complete-module-read "Module: " modules)))
                       ((= (length modules) 1)
                        (let ((module (car modules)))
                          (unless (member module suggested-already)
                            (haskell-process-set-suggested-imports process (cons module suggested-already))
                            (when (y-or-n-p (format "Identifier `%s' not in scope, import `%s'?"
                                                    ident
                                                    module))
                              module)))))))
    (when module
      (haskell-process-find-file session file)
      (save-excursion
        (goto-char (point-max))
        (haskell-navigate-imports)
        (insert (read-from-minibuffer "Import line: " (concat "import " module))
                "\n")
        (haskell-sort-imports)
        (haskell-align-imports)))))

(defun haskell-process-trigger-suggestions (session msg file line)
  "Trigger prompting to add any extension suggestions."
  (cond ((let ((case-fold-search nil))
           (or (and (string-match " -X\\([A-Z][A-Za-z]+\\)" msg)
                    (not (string-match "\\([A-Z][A-Za-z]+\\) is deprecated" msg)))
               (string-match "Use \\([A-Z][A-Za-z]+\\) to permit this" msg)
               (string-match "Use \\([A-Z][A-Za-z]+\\) to allow" msg)
               (string-match "use \\([A-Z][A-Za-z]+\\)" msg)
               (string-match "You need \\([A-Z][A-Za-z]+\\)" msg)))
         (when haskell-process-suggest-language-pragmas
           (haskell-process-suggest-pragma session "LANGUAGE" (match-string 1 msg) file)))
        ((string-match " The \\(qualified \\)?import of[ ][‘`‛]\\([^ ]+\\)['’] is redundant" msg)
         (when haskell-process-suggest-remove-import-lines
           (haskell-process-suggest-remove-import session
                                                  file
                                                  (match-string 2 msg)
                                                  line)))
        ((string-match "Warning: orphan instance: " msg)
         (when haskell-process-suggest-no-warn-orphans
           (haskell-process-suggest-pragma session "OPTIONS" "-fno-warn-orphans" file)))
        ((or (string-match "against inferred type [‘`‛]\\[Char\\]['’]" msg)
             (string-match "with actual type [‘`‛]\\[Char\\]['’]" msg))
         (when haskell-process-suggest-overloaded-strings
           (haskell-process-suggest-pragma session "LANGUAGE" "OverloadedStrings" file)))
        ((string-match "^Not in scope: .*[‘`‛]\\(.+\\)['’]$" msg)
         (let* ((match1 (match-string 1 msg))
                (ident (if (string-match "^[A-Za-z0-9_'.]+\\.\\(.+\\)$" match1)
                           ;; Skip qualification.
                           (match-string 1 match1)
                         match1)))
           (when haskell-process-suggest-hoogle-imports
             (let ((modules (haskell-process-hoogle-ident ident)))
               (haskell-process-suggest-imports session file modules ident)))
           (when haskell-process-suggest-haskell-docs-imports
             (let ((modules (haskell-process-haskell-docs-ident ident)))
               (haskell-process-suggest-imports session file modules ident)))
           (when haskell-process-suggest-hayoo-imports
             (let ((modules (haskell-process-hayoo-ident ident)))
               (haskell-process-suggest-imports session file modules ident)))))
        ((string-match "^[ ]+It is a member of the hidden package [‘`‛]\\(.+\\)['’].$" msg)
         (when haskell-process-suggest-add-package
           (haskell-process-suggest-add-package session msg)))))

(defun haskell-process ()
  "Get the current process from the current session."
  (haskell-session-process (haskell-session)))

(defun haskell-interactive-buffer ()
  "Get the interactive buffer of the session."
  (haskell-session-interactive-buffer (haskell-session)))

(defun haskell-interactive-jump-to-error-line ()
  "Jump to the error line."
  (let ((orig-line (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))
    (and (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)\\(-[0-9]+\\)?:" orig-line)
         (let* ((file (match-string 1 orig-line))
                (line (match-string 2 orig-line))
                (col (match-string 3 orig-line))
                (session (haskell-session))
                (cabal-path (haskell-session-cabal-dir session))
                (src-path (haskell-session-current-dir session))
                (cabal-relative-file (expand-file-name file cabal-path))
                (src-relative-file (expand-file-name file src-path)))
           (let ((file (cond ((file-exists-p cabal-relative-file)
                              cabal-relative-file)
                             ((file-exists-p src-relative-file)
                              src-relative-file))))
             (when file
               (other-window 1)
               (find-file file)
               (haskell-interactive-bring)
               (goto-char (point-min))
               (forward-line (1- (string-to-number line)))
               (goto-char (+ (point) (string-to-number col) -1))
               (haskell-mode-message-line orig-line)
               t))))))

;;;###autoload
(defun haskell-interactive-mode-echo (session message &optional mode)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (if mode
                  (haskell-fontify-as-mode
                   (concat message "\n")
                   mode)
                (propertize (concat message "\n")
                            'read-only t
                            'rear-nonsticky t))))))

(defun haskell-interactive-mode-compile-splice (session message)
  "Echo a compiler splice."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (setq next-error-last-buffer (current-buffer))
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (haskell-fontify-as-mode message 'haskell-mode)
              "\n"))))

(defun haskell-interactive-mode-insert-garbage (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (propertize message
                          'font-lock-face 'haskell-interactive-face-garbage
                          'read-only t
                          'rear-nonsticky t)))))

(defun haskell-process-do-cabal (command)
  "Run a Cabal command."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list (haskell-session) process command 0)

      :go
      (lambda (state)
        (haskell-process-send-string
         (cadr state)
         (format haskell-process-do-cabal-format-string
                 (haskell-session-cabal-dir (car state))
                 (format "%s %s"
                         (cl-ecase (haskell-process-type)
                           ('ghci haskell-process-path-cabal)
                           ('cabal-repl haskell-process-path-cabal)
                           ('cabal-ghci haskell-process-path-cabal)
                           ('cabal-dev haskell-process-path-cabal-dev))
                         (cl-caddr state)))))

      :live
      (lambda (state buffer)
        (let ((cmd (replace-regexp-in-string "^\\([a-z]+\\).*"
                                             "\\1"
                                             (cl-caddr state))))
          (cond ((or (string= cmd "build")
                     (string= cmd "install"))
                 (haskell-process-live-build (cadr state) buffer t))
                (t
                 (haskell-process-cabal-live state buffer)))))

      :complete
      (lambda (state response)
        (let* ((process (cadr state))
               (session (haskell-process-session process))
               (message-count 0)
               (cursor (haskell-process-response-cursor process)))
          (haskell-process-set-response-cursor process 0)
          (while (haskell-process-errors-warnings session process response)
            (setq message-count (1+ message-count)))
          (haskell-process-set-response-cursor process cursor)
          (let ((msg (format "Complete: cabal %s (%s compiler messages)"
                             (cl-caddr state)
                             message-count)))
            (haskell-interactive-mode-echo session msg)
            (when (= message-count 0)
              (haskell-interactive-mode-echo
               session
               "No compiler messages, dumping complete output:")
              (haskell-interactive-mode-echo session response))
            (haskell-mode-message-line msg)
            (when (and haskell-notify-p
                       (fboundp 'notifications-notify))
              (notifications-notify
               :title (format "*%s*" (haskell-session-name (car state)))
               :body msg
               :app-name (cl-ecase (haskell-process-type)
                           ('ghci haskell-process-path-cabal)
                           ('cabal-repl haskell-process-path-cabal)
                           ('cabal-ghci haskell-process-path-cabal)
                           ('cabal-dev haskell-process-path-cabal-dev))
               :app-icon haskell-process-logo
               )))))))))

(defun haskell-process-look-config-changes (session)
  "Checks whether a cabal configuration file has
changed. Restarts the process if that is the case."
  (let ((current-checksum (haskell-session-get session 'cabal-checksum))
        (new-checksum (haskell-cabal-compute-checksum
                       (haskell-session-get session 'cabal-dir))))
    (when (not (string= current-checksum new-checksum))
      (haskell-interactive-mode-echo session (format "Cabal file changed: %s" new-checksum))
      (haskell-session-set-cabal-checksum session
                                          (haskell-session-get session 'cabal-dir))
      (unless (and haskell-process-prompt-restart-on-cabal-change
                   (not (y-or-n-p "Cabal file changed; restart GHCi process? ")))
        (haskell-process-start (haskell-session))))))

(defun haskell-process-start (session)
  "Start the inferior Haskell process."
  (let ((existing-process (get-process (haskell-session-name (haskell-session)))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Restarting process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process)))
  (let ((process (or (haskell-session-process session)
                     (haskell-process-make (haskell-session-name session))))
        (old-queue (haskell-process-get (haskell-session-process session)
                                        'command-queue)))
    (haskell-session-set-process session process)
    (haskell-process-set-session process session)
    (haskell-process-set-cmd process nil)
    (haskell-process-set (haskell-session-process session) 'is-restarting nil)
    (let ((default-directory (haskell-session-cabal-dir session)))
      (haskell-session-pwd session)
      (haskell-process-set-process
       process
       (cl-ecase (haskell-process-type)
         ('ghci
          (haskell-process-log
           (propertize (format "Starting inferior GHCi process %s ..."
                               haskell-process-path-ghci)
                       'face font-lock-comment-face))
          (apply #'start-process
                 (append (list (haskell-session-name session)
                               nil
                               haskell-process-path-ghci)
                         haskell-process-args-ghci)))
         ('cabal-repl
          (haskell-process-log
           (propertize
            (format "Starting inferior `cabal repl' process using %s ..."
                    haskell-process-path-cabal)
            'face font-lock-comment-face))

          (apply #'start-process
                 (append (list (haskell-session-name session)
                               nil
                               haskell-process-path-cabal)
                         '("repl") haskell-process-args-cabal-repl
                         (let ((target (haskell-session-target session)))
                           (if target (list target) nil)))))
         ('cabal-ghci
          (haskell-process-log
           (propertize
            (format "Starting inferior cabal-ghci process using %s ..."
                    haskell-process-path-cabal-ghci)
            'face font-lock-comment-face))
          (start-process (haskell-session-name session)
                         nil
                         haskell-process-path-cabal-ghci))
         ('cabal-dev
          (let ((dir (concat (haskell-session-cabal-dir session)
                             "/cabal-dev")))
            (haskell-process-log
             (propertize (format "Starting inferior cabal-dev process %s -s %s ..."
                                 haskell-process-path-cabal-dev
                                 dir)
                         'face font-lock-comment-face))
            (start-process (haskell-session-name session)
                           nil
                           haskell-process-path-cabal-dev
                           "ghci"
                           "-s"
                           dir))))))
    (progn (set-process-sentinel (haskell-process-process process) 'haskell-process-sentinel)
           (set-process-filter (haskell-process-process process) 'haskell-process-filter))
    (haskell-process-send-startup process)
    (unless (eq 'cabal-repl (haskell-process-type)) ;; "cabal repl" sets the proper CWD
      (haskell-process-change-dir session
                                  process
                                  (haskell-session-current-dir session)))
    (haskell-process-set process 'command-queue
                         (append (haskell-process-get (haskell-session-process session)
                                                      'command-queue)
                                 old-queue))
    process))

(defun haskell-kill-session-process (&optional session)
  "Kill the process."
  (interactive)
  (let* ((session (or session (haskell-session)))
         (existing-process (get-process (haskell-session-name session))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Killing process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process))))

(defun haskell-interactive-show-load-message (session type module-name file-name echo th)
  "Show the '(Compiling|Loading) X' message."
  (let ((msg (concat
              (cl-ecase type
                ('compiling
                 (if haskell-interactive-mode-include-file-name
                     (format "Compiling: %s (%s)" module-name file-name)
                   (format "Compiling: %s" module-name)))
                ('loading (format "Loading: %s" module-name))
                ('import-cycle (format "Module has an import cycle: %s" module-name)))
              (if th " [TH]" ""))))
    (haskell-mode-message-line msg)
    (when haskell-interactive-mode-delete-superseded-errors
      (haskell-interactive-mode-delete-compile-messages session file-name))
    (when echo
      (haskell-interactive-mode-echo session msg))))

(defun haskell-interactive-mode-visit-error ()
  "Visit the buffer of the current (or last) error message."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (if (progn (goto-char (line-beginning-position))
               (looking-at haskell-interactive-mode-error-regexp))
        (progn (forward-line -1)
               (haskell-interactive-jump-to-error-line))
      (progn (goto-char (point-max))
             (haskell-interactive-mode-error-backward)
             (haskell-interactive-jump-to-error-line)))))

;;;###autoload
(defun haskell-process-do-simple-echo (line &optional mode)
  "Send LINE to the GHCi process and echo the result in some
fashion, such as printing in the minibuffer, or using
haskell-present, depending on configuration."
  (let ((process (haskell-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list process line mode)
      :go (lambda (state)
            (haskell-process-send-string (car state) (cadr state)))
      :complete (lambda (state response)
                  ;; TODO: TBD: don't do this if
                  ;; `haskell-process-use-presentation-mode' is t.
                  (haskell-interactive-mode-echo
                   (haskell-process-session (car state))
                   response
                   (cl-caddr state))
                  (if haskell-process-use-presentation-mode
                      (progn (haskell-present (cadr state)
                                              (haskell-process-session (car state))
                                              response)
                             (haskell-session-assign
                              (haskell-process-session (car state))))
                    (haskell-mode-message-line response)))))))

;;;###autoload
(defun haskell-process-do-type (&optional insert-value)
  "Print the type of the given expression."
  (interactive "P")
  (if insert-value
      (haskell-process-insert-type)
    (haskell-process-do-simple-echo
     (let ((ident (haskell-ident-at-point)))
       ;; TODO: Generalize all these `string-match' of ident calls into
       ;; one function.
       (format (if (string-match "^[_[:lower:][:upper:]]" ident)
                   ":type %s"
                 ":type (%s)")
               ident))
     'haskell-mode)))

;;;###autoload
(defun haskell-process-do-info (&optional prompt-value)
  "Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer."
  (interactive "P")
  (haskell-process-do-simple-echo
   (let ((ident (if prompt-value
                    (read-from-minibuffer "Info: " (haskell-ident-at-point))
                  (haskell-ident-at-point)))
         (modname (unless prompt-value
                    (haskell-utils-parse-import-statement-at-point))))
     (if modname
         (format ":browse! %s" modname)
       (format (if (string-match "^[a-zA-Z_]" ident)
                   ":info %s"
                 ":info (%s)")
               (or ident
                   (haskell-ident-at-point)))))
   'haskell-mode))

(defvar haskell-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'haskell-interactive-mode-return)
    (define-key map (kbd "SPC") 'haskell-interactive-mode-space)
    (define-key map (kbd "C-j") 'haskell-interactive-mode-newline-indent)
    (define-key map (kbd "C-a") 'haskell-interactive-mode-beginning)
    (define-key map (kbd "<home>") 'haskell-interactive-mode-beginning)
    (define-key map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key map (kbd "C-c C-c") 'haskell-process-interrupt)
    (define-key map (kbd "C-c C-f") 'next-error-follow-minor-mode)
    (define-key map (kbd "C-c C-z") 'haskell-interactive-switch-back)
    (define-key map (kbd "M-p") 'haskell-interactive-mode-history-previous)
    (define-key map (kbd "M-n") 'haskell-interactive-mode-history-next)
    (define-key map (kbd "C-<up>") 'haskell-interactive-mode-history-previous)
    (define-key map (kbd "C-<down>") 'haskell-interactive-mode-history-next)
    (define-key map (kbd "TAB") 'haskell-interactive-mode-tab)
    (define-key map (kbd "<C-S-backspace>") 'haskell-interactive-mode-kill-whole-line)
    map)
  "Interactive Haskell mode map.")

(defun haskell-mode-contextual-space ()
  "Contextually do clever stuff when hitting space."
  (interactive)
  (if (or (not (bound-and-true-p interactive-haskell-mode))
          (not (haskell-session-maybe)))
      (self-insert-command 1)
    (cond ((and haskell-mode-contextual-import-completion
                (save-excursion (forward-word -1)
                                (looking-at "^import$")))
           (insert " ")
           (let ((module (haskell-complete-module-read
                          "Module: "
                          (haskell-session-all-modules (haskell-session)))))
             (insert module)
             (haskell-mode-format-imports)))
          ((not (string= "" (save-excursion (forward-char -1) (haskell-ident-at-point))))
           (let ((ident (save-excursion (forward-char -1) (haskell-ident-at-point))))
             (insert " ")
             (haskell-process-do-try-info ident)))
          (t (insert " ")))))

(defun haskell-mode-jump-to-tag (&optional next-p)
  "Jump to the tag of the given identifier."
  (interactive "P")
  (let ((ident (haskell-ident-at-point))
        (tags-file-name (haskell-session-tags-filename (haskell-session)))
        (tags-revert-without-query t))
    (when (not (string= "" (haskell-trim ident)))
      (cond ((file-exists-p tags-file-name)
             (find-tag ident next-p))
            (t (haskell-process-generate-tags ident))))))

(defun haskell-mode-after-save-handler ()
  "Function that will be called after buffer's saving."
  (when haskell-tags-on-save
    (ignore-errors (when (and (boundp 'haskell-session) haskell-session)
                     (haskell-process-generate-tags))))
  (when haskell-stylish-on-save
    (ignore-errors (haskell-mode-stylish-buffer))
    (let ((before-save-hook '())
          (after-save-hook '()))
      (basic-save-buffer))))

(defun haskell-mode-buffer-apply-command (cmd)
  "Execute shell command CMD with current buffer as input and
replace the whole buffer with the output. If CMD fails the buffer
remains unchanged."
  (set-buffer-modified-p t)
  (let* ((chomp (lambda (str)
                  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
                    (setq str (replace-match "" t t str)))
                  str))
         (errout (lambda (fmt &rest args)
                   (let* ((warning-fill-prefix "    "))
                     (display-warning cmd (apply 'format fmt args) :warning))))
         (filename (buffer-file-name (current-buffer)))
         (cmd-prefix (replace-regexp-in-string " .*" "" cmd))
         (tmp-file (make-temp-file cmd-prefix))
         (err-file (make-temp-file cmd-prefix))
         (default-directory (if (and (boundp 'haskell-session)
                                     haskell-session)
                                (haskell-session-cabal-dir haskell-session)
                              default-directory))
         (errcode (with-temp-file tmp-file
                    (call-process cmd filename
                                  (list (current-buffer) err-file) nil)))
         (stderr-output
          (with-temp-buffer
            (insert-file-contents err-file)
            (funcall chomp (buffer-substring-no-properties (point-min) (point-max)))))
         (stdout-output
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string= "" stderr-output)
        (if (string= "" stdout-output)
            (funcall errout
                     "Error: %s produced no output, leaving buffer alone" cmd)
          (save-restriction
            (widen)
            ;; command successful, insert file with replacement to preserve
            ;; markers.
            (insert-file-contents tmp-file nil nil nil t)))
      ;; non-null stderr, command must have failed
      (funcall errout "%s failed: %s" cmd stderr-output))
    (delete-file tmp-file)
    (delete-file err-file)))

(defun haskell-mode-stylish-buffer ()
  "Apply stylish-haskell to the current buffer."
  (interactive)
  (let ((column (current-column))
        (line (line-number-at-pos)))
    (haskell-mode-buffer-apply-command "stylish-haskell")
    (goto-char (point-min))
    (forward-line (1- line))
    (goto-char (+ column (point)))))

(defun haskell-mode-jump-to-filename-in-string ()
  "Jump to the filename in the current string."
  (let* ((string (save-excursion
                   (buffer-substring-no-properties
                    (1+ (search-backward-regexp "\"" (line-beginning-position) nil 1))
                    (1- (progn (forward-char 1)
                               (search-forward-regexp "\"" (line-end-position) nil 1))))))
         (fp (expand-file-name string
                               (haskell-session-cabal-dir (haskell-session)))))
    (find-file
     (read-file-name
      ""
      fp
      fp))))

(defun haskell-mode-tag-find (&optional next-p)
  "The tag find function, specific for the particular session."
  (interactive "P")
  (cond
   ((elt (syntax-ppss) 3) ;; Inside a string
    (haskell-mode-jump-to-filename-in-string))
   (t (call-interactively 'haskell-mode-jump-to-tag))))

(defun haskell-rgrep (&optional prompt)
  "Grep the effective project for the symbol at point. Very
useful for codebase navigation. Prompts for an arbitrary regexp
given a prefix arg."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (rgrep sym
           "*.hs" ;; TODO: common Haskell extensions.
           (haskell-session-current-dir (haskell-session)))))

(defun haskell-describe (ident)
  "Describe the given identifier."
  (interactive (list (read-from-minibuffer "Describe identifier: "
                                           (haskell-ident-at-point))))
  (let ((results (read (shell-command-to-string
                        (concat "haskell-docs --sexp "
                                ident)))))
    (help-setup-xref (list #'haskell-describe ident)
		     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (if results
              (cl-loop for result in results
                       do (insert (propertize ident 'font-lock-face
                                              '((:inherit font-lock-type-face
                                                          :underline t)))
                                  " is defined in "
                                  (let ((module (cadr (assoc 'module result))))
                                    (if module
                                        (concat module " ")
                                      ""))
                                  (cadr (assoc 'package result))
                                  "\n\n")
                       do (let ((type (cadr (assoc 'type result))))
                            (when type
                              (insert (haskell-fontify-as-mode type 'haskell-mode)
                                      "\n")))
                       do (let ((args (cadr (assoc 'type results))))
                            (cl-loop for arg in args
                                     do (insert arg "\n"))
                            (insert "\n"))
                       do (insert (cadr (assoc 'documentation result)))
                       do (insert "\n\n"))
            (insert "No results for " ident)))))))

(defun haskell-process-extract-modules (buffer)
  "Extract the modules from the process buffer."
  (let* ((modules-string (match-string 1 buffer))
         (modules (split-string modules-string ", ")))
    (cons modules modules-string)))

(defun haskell-process-import-modules (process modules)
  "Import `modules' with :m +, and send any import statements
from `module-buffer'."
  (when haskell-process-auto-import-loaded-modules
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process modules)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (format ":m + %s" (mapconcat 'identity (cdr state) " "))))))))

(defun haskell-session-strip-dir (session file)
  "Strip the load dir from the file path."
  (let ((cur-dir (haskell-session-current-dir session)))
    (if (> (length file) (length cur-dir))
        (if (string= (substring file 0 (length cur-dir))
                     cur-dir)
            (replace-regexp-in-string
             "^[/\\]" ""
             (substring file
                        (length cur-dir)))
          file)
      file)))

(defun haskell-process-haskell-docs-ident (ident)
  "Search with haskell-docs for IDENT, returns a list of modules."
  (cl-remove-if-not (lambda (a) (string-match "^[A-Z][A-Za-b0-9_'.]+$" a))
                    (split-string (shell-command-to-string (concat "haskell-docs --modules " ident))
                                  "\n")))

(defun haskell-process-hoogle-ident (ident)
  "Hoogle for IDENT, returns a list of modules."
  (with-temp-buffer
    (let ((hoogle-error (call-process "hoogle" nil t nil "search" "--exact" ident)))
      (goto-char (point-min))
      (unless (or (/= 0 hoogle-error)
                  (looking-at "^No results found")
                  (looking-at "^package "))
        (while (re-search-forward "^\\([^ ]+\\).*$" nil t)
          (replace-match "\\1" nil nil))
        (cl-remove-if (lambda (a) (string= "" a))
                      (split-string (buffer-string)
                                    "\n"))))))

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defun haskell-process-hayoo-ident (ident)
  "Hayoo for IDENT, returns a list of modules asyncronously through CALLBACK."
  ;; We need a real/simulated closure, because otherwise these
  ;; variables will be unbound when the url-retrieve callback is
  ;; called.
  ;; TODO: Remove when this code is converted to lexical bindings by
  ;; default (Emacs 24.1+)
  (let ((url (format haskell-process-hayoo-query-url (url-hexify-string ident))))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (= 200 url-http-response-status)
          (progn
            (goto-char url-http-end-of-headers)
            (let* ((res (json-read))
                   (results (assoc-default 'result res)))
              ;; TODO: gather packages as well, and when we choose a
              ;; given import, check that we have the package in the
              ;; cabal file as well.
              (cl-mapcan (lambda (r)
                           ;; append converts from vector -> list
                           (append (assoc-default 'resultModules r) nil))
                         results)))
        (warn "HTTP error %s fetching %s" url-http-response-status url)))))

(provide 'haskell)
