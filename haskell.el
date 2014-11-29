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

(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-interactive-mode)
(require 'haskell-presentation-mode)

(defvar haskell-interactive-mode-old-prompt-start
  nil
  "Mark used for the old beginning of the prompt.")

(add-hook 'haskell-process-ended-hook 'haskell-process-prompt-restart)

(define-derived-mode haskell-debug-mode
  text-mode "Debug"
  "Major mode for debugging Haskell via GHCi.")

(defvar haskell-debug-history-cache nil
  "Cache of the tracing history.")

(defvar haskell-debug-bindings-cache nil
  "Cache of the current step's bindings.")


(define-key haskell-debug-mode-map (kbd "g") 'haskell-debug/refresh)
(define-key haskell-debug-mode-map (kbd "s") 'haskell-debug/step)
(define-key haskell-debug-mode-map (kbd "d") 'haskell-debug/delete)
(define-key haskell-debug-mode-map (kbd "b") 'haskell-debug/break-on-function)
(define-key haskell-debug-mode-map (kbd "a") 'haskell-debug/abandon)
(define-key haskell-debug-mode-map (kbd "c") 'haskell-debug/continue)
(define-key haskell-debug-mode-map (kbd "p") 'haskell-debug/previous)
(define-key haskell-debug-mode-map (kbd "n") 'haskell-debug/next)
(define-key haskell-debug-mode-map (kbd "RET") 'haskell-debug/select)

(defun haskell-debug/start-step (expr)
  "Start stepping EXPR."
  (interactive (list (read-from-minibuffer "Expression to step through: ")))
  (haskell-debug/step expr))

(defun haskell-debug/refresh ()
  "Refresh the debugger buffer."
  (interactive)
  (with-current-buffer (haskell-debug-buffer-name (haskell-session))
    (let ((inhibit-read-only t)
          (p (point)))
      (erase-buffer)
      (insert (propertize (concat "Debugging "
                                  (haskell-session-name (haskell-session))
                                  "\n\n")
                          'face `((:weight bold))))
      (let ((modules (haskell-debug-get-modules))
            (breakpoints (haskell-debug-get-breakpoints))
            (context (haskell-debug-get-context))
            (history (haskell-debug-get-history)))
        (unless modules
          (insert (propertize "You have to load a module to start debugging."
                              'face
                              'haskell-debug-warning-face)
                  "\n\n"))
        (haskell-debug-insert-bindings modules breakpoints context)
        (when modules
          (haskell-debug-insert-current-context context history)
          (haskell-debug-insert-breakpoints breakpoints))
        (haskell-debug-insert-modules modules))
      (insert "\n")
      (goto-char (min (point-max) p)))))

(defun haskell-debug-insert-context (context history)
  "Insert the context and history."
  (when context
    (insert (propertize (plist-get context :name) 'face `((:weight bold)))
            (haskell-debug-muted " - ")
            (file-name-nondirectory (plist-get context :path))
            (haskell-debug-muted " (stopped)")
            "\n"))
  (when haskell-debug-bindings-cache
    (insert "\n")
    (let ((bindings haskell-debug-bindings-cache))
      (insert
       (haskell-debug-get-span-string
        (plist-get bindings :path)
        (plist-get bindings :span)))
      (insert "\n\n")
      (cl-loop for binding in (plist-get bindings :types)
               do (insert (haskell-fontify-as-mode binding 'haskell-mode)
                          "\n"))))
  (let ((history (or history
                     (list (haskell-debug-make-fake-history context)))))
    (when history
      (insert "\n")
      (haskell-debug-insert-history history))))

(defun haskell-debug-insert-current-context (context history)
  "Insert the current context."
  (haskell-debug-insert-header "Context")
  (if context
      (haskell-debug-insert-context context history)
    (haskell-debug-insert-debug-finished))
  (insert "\n"))

(defun haskell-debug-preview-span (span string &optional collapsed)
  "Make a one-line preview of the given expression."
  (with-temp-buffer
    (haskell-mode)
    (insert string)
    (when (/= 0 (plist-get span :start-col))
      (indent-rigidly (point-min)
                      (point-max)
                      1))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (when (/= 0 (plist-get span :start-col))
      (indent-rigidly (point-min)
                      (point-max)
                      -1))
    (goto-char (point-min))
    (if collapsed
        (replace-regexp-in-string
         "\n[ ]*"
         (propertize " " 'face 'haskell-debug-newline-face)
         (buffer-substring (point-min)
                           (point-max)))
      (buffer-string))))

(defun haskell-debug-insert-history (history)
  "Insert tracing HISTORY."
  (let ((i (length history)))
    (cl-loop for span in history
             do (let ((string (haskell-debug-get-span-string
                               (plist-get span :path)
                               (plist-get span :span)))
                      (index (plist-get span :index)))
                  (insert (propertize (format "%4d" i)
                                      'face 'haskell-debug-trace-number-face)
                          " "
                          (haskell-debug-preview-span
                           (plist-get span :span)
                           string
                           t)
                          "\n")
                  (setq i (1- i))))))

(defun haskell-debug-insert-debug-finished ()
  "Insert message that no debugging is happening, but if there is
some old history, then display that."
  (if haskell-debug-history-cache
      (progn (haskell-debug-insert-muted "Finished debugging.")
             (insert "\n")
             (haskell-debug-insert-history haskell-debug-history-cache))
    (haskell-debug-insert-muted "Not debugging right now.")))

(defun haskell-debug/breakpoint-numbers ()
  "List breakpoint numbers."
  (interactive)
  (let ((breakpoints (mapcar (lambda (breakpoint)
                               (number-to-string (plist-get breakpoint :number)))
                             (haskell-debug-get-breakpoints))))
    (if (null breakpoints)
        (message "No breakpoints.")
      (message "Breakpoint(s): %s"
               (mapconcat #'identity
                          breakpoints
                          ", ")))))

(defun haskell-debug/next ()
  "Go to next step to inspect bindings."
  (interactive)
  (haskell-debug-with-breakpoints
   (haskell-debug-navigate "forward")))

(defun haskell-debug/previous ()
  "Go to previous step to inspect the bindings."
  (interactive)
  (haskell-debug-with-breakpoints
   (haskell-debug-navigate "back")))

(defun haskell-debug-start (session)
  "Start the debug mode."
  (setq buffer-read-only t)
  (haskell-session-assign session)
  (haskell-debug/refresh))

(add-hook 'kill-buffer-hook 'haskell-interactive-kill)

(defun haskell-debug ()
  "Start the debugger for the current Haskell (GHCi) session."
  (interactive)
  (let ((session (haskell-session)))
    (switch-to-buffer-other-window (haskell-debug-buffer-name session))
    (unless (eq major-mode 'haskell-debug-mode)
      (haskell-debug-mode)
      (haskell-debug-start session))))

(defun haskell-debug/delete ()
  "Delete whatever's at the point."
  (interactive)
  (cond
   ((get-text-property (point) 'break)
    (let ((break (get-text-property (point) 'break)))
      (when (y-or-n-p (format "Delete breakpoint #%d?"
                              (plist-get break :number)))
        (haskell-process-queue-sync-request
         (haskell-process)
         (format ":delete %d"
                 (plist-get break :number)))
        (haskell-debug/refresh))))))

(defun haskell-debug/step (&optional expr)
  "Step into the next function."
  (interactive)
  (haskell-debug-with-breakpoints
   (let* ((breakpoints (haskell-debug-get-breakpoints))
          (context (haskell-debug-get-context))
          (string
           (haskell-process-queue-sync-request
            (haskell-process)
            (if expr
                (concat ":step " expr)
              ":step"))))
     (cond
      ((string= string "not stopped at a breakpoint\n")
       (if haskell-debug-bindings-cache
           (progn (setq haskell-debug-bindings-cache nil)
                  (haskell-debug/refresh))
         (call-interactively 'haskell-debug/start-step)))
      (t (let ((maybe-stopped-at (haskell-debug-parse-stopped-at string)))
           (cond
            (maybe-stopped-at
             (set (make-local-variable 'haskell-debug-bindings-cache)
                  maybe-stopped-at)
             (message "Computation paused.")
             (haskell-debug/refresh))
            (t
             (if context
                 (message "Computation finished.")
               (when (y-or-n-p "Computation completed without breaking. Reload the module and retry?")
                 (message "Reloading and resetting breakpoints...")
                 (haskell-interactive-mode-reset-error (haskell-session))
                 (cl-loop for break in breakpoints
                          do (haskell-process-file-loadish
                              (concat "load " (plist-get break :path))
                              nil
                              nil))
                 (cl-loop for break in breakpoints
                          do (haskell-debug-break break))
                 (haskell-debug/step expr)))))))))
   (haskell-debug/refresh)))

(defun haskell-debug-break (break)
  "Set BREAK breakpoint in module at line/col."
  (haskell-process-queue-without-filters
   (haskell-process)
   (format ":break %s %s %d"
           (plist-get break :module)
           (plist-get (plist-get break :span) :start-line)
           (plist-get (plist-get break :span) :start-col))))

(defun haskell-debug/abandon ()
  "Abandon the current computation."
  (interactive)
  (haskell-debug-with-breakpoints
   (haskell-process-queue-sync-request (haskell-process) ":abandon")
   (message "Computation abandoned.")
   (setq haskell-debug-history-cache nil)
   (setq haskell-debug-bindings-cache nil)
   (haskell-debug/refresh)))

(defun haskell-debug/continue ()
  "Continue the current computation."
  (interactive)
  (haskell-debug-with-breakpoints
   (haskell-process-queue-sync-request (haskell-process) ":continue")
   (message "Computation continued.")
   (setq haskell-debug-history-cache nil)
   (setq haskell-debug-bindings-cache nil)
   (haskell-debug/refresh)))

(defun haskell-debug/break-on-function ()
  "Break on function IDENT."
  (interactive)
  (haskell-debug-with-modules
   (let ((ident (read-from-minibuffer "Function: "
                                      (haskell-ident-at-point))))
     (haskell-process-queue-sync-request
      (haskell-process)
      (concat ":break "
              ident))
     (message "Breaking on function: %s" ident)
     (haskell-debug/refresh))))

(defun haskell-debug-get-modules ()
  "Get the list of modules currently set."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-process)
                 ":show modules")))
    (if (string= string "")
        (list)
      (mapcar #'haskell-debug-parse-module
              (haskell-debug-split-string string)))))

(defun haskell-debug-get-context ()
  "Get the current context."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-process)
                 ":show context")))
    (if (string= string "")
        nil
      (haskell-debug-parse-context string))))

(defun haskell-debug-navigate (direction)
  "Navigate in DIRECTION \"back\" or \"forward\"."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-process)
                 (concat ":" direction))))
    (let ((bindings (haskell-debug-parse-logged string)))
      (set (make-local-variable 'haskell-debug-bindings-cache)
           bindings)
      (when (not bindings)
        (message "No more %s results!" direction)))
    (haskell-debug/refresh)))

(defun haskell-debug-get-history ()
  "Get the step history."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-process)
                 ":history")))
    (if (or (string= string "")
            (string= string "Not stopped at a breakpoint\n"))
        nil
      (if (string= string "Empty history. Perhaps you forgot to use :trace?\n")
          nil
        (let ((entries (mapcar #'haskell-debug-parse-history-entry
                               (cl-remove-if (lambda (line) (or (string= "<end of history>" line)
                                                                (string= "..." line)))
                                             (haskell-debug-split-string string)))))
          (set (make-local-variable 'haskell-debug-history-cache)
               entries)
          entries)))))

(defun haskell-debug-get-breakpoints ()
  "Get the list of breakpoints currently set."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-process)
                 ":show breaks")))
    (if (string= string "No active breakpoints.\n")
        (list)
      (mapcar #'haskell-debug-parse-break-point
              (haskell-debug-split-string string)))))

(defun haskell-interactive-mode-insert-error (response)
  "Insert an error message."
  (insert "\n"
          (haskell-fontify-as-mode
           response
           'haskell-mode))
  (haskell-interactive-mode-prompt))

(defun haskell-interactive-mode-trigger-compile-error (state response)
  "Look for an <interactive> compile error; if there is one, pop
  that up in a buffer, similar to `debug-on-error'."
  (when (and haskell-interactive-types-for-show-ambiguous
             (string-match "^\n<interactive>:[0-9]+:[0-9]+:" response)
             (not (string-match "^\n<interactive>:[0-9]+:[0-9]+:[\n ]+Warning:" response)))
    (let ((inhibit-read-only t))
      (delete-region haskell-interactive-mode-prompt-start (point))
      (set-marker haskell-interactive-mode-prompt-start
                  haskell-interactive-mode-old-prompt-start)
      (goto-char (point-max)))
    (cond
     ((and (not (haskell-interactive-mode-line-is-query (elt state 2)))
           (or (string-match "No instance for (?Show[ \n]" response)
               (string-match "Ambiguous type variable " response)))
      (haskell-process-reset (haskell-process))
      (let ((resp (haskell-process-queue-sync-request
                   (haskell-process)
                   (concat ":t "
                           (buffer-substring-no-properties
                            haskell-interactive-mode-prompt-start
                            (point-max))))))
        (cond
         ((not (string-match "<interactive>:" resp))
          (haskell-interactive-mode-insert-error resp))
         (t (haskell-interactive-popup-error response)))))
     (t (haskell-interactive-popup-error response)
        t))
    t))
(defun haskell-interactive-mode-run-expr (expr)
  "Run the given expression."
  (let ((session (haskell-session))
        (process (haskell-process))
        (lines (length (split-string expr "\n"))))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list session process expr 0)
      :go (lambda (state)
            (goto-char (point-max))
            (insert "\n")
            (setq haskell-interactive-mode-result-end
                  (point-max))
            (haskell-process-send-string (cadr state)
                                         (haskell-interactive-mode-multi-line (cl-caddr state)))
            (haskell-process-set-evaluating (cadr state) t))
      :live (lambda (state buffer)
              (unless (and (string-prefix-p ":q" (cl-caddr state))
                           (string-prefix-p (cl-caddr state) ":quit"))
                (let* ((cursor (cl-cadddr state))
                       (next (replace-regexp-in-string
                              haskell-process-prompt-regex
                              ""
                              (substring buffer cursor))))
                  (haskell-interactive-mode-eval-result (car state) next)
                  (setf (cl-cdddr state) (list (length buffer)))
                  nil)))
      :complete
      (lambda (state response)
        (haskell-process-set-evaluating (cadr state) nil)
        (unless (haskell-interactive-mode-trigger-compile-error state response)
          (haskell-interactive-mode-expr-result state response)))))))

(defun haskell-interactive-mode-do-expr (expr)
  (cond
   ((string-match "^:present " expr)
    (haskell-interactive-mode-do-presentation (replace-regexp-in-string "^:present " "" expr)))
   (t
    (haskell-interactive-mode-run-expr expr))))

(defun haskell-interactive-handle-expr ()
  "Handle an inputted expression at the REPL."
  (when (haskell-interactive-at-prompt)
    (let ((expr (haskell-interactive-mode-input)))
      (unless (string= "" (replace-regexp-in-string " " "" expr))
        (cond
         ;; If already evaluating, then the user is trying to send
         ;; input to the REPL during evaluation. Most likely in
         ;; response to a getLine-like function.
         ((and (haskell-process-evaluating-p (haskell-process))
               (= (line-end-position) (point-max)))
          (goto-char (point-max))
          (let ((process (haskell-process))
                (string (buffer-substring-no-properties
                         haskell-interactive-mode-result-end
                         (point))))
            (insert "\n")
            ;; Bring the marker forward
            (setq haskell-interactive-mode-result-end
                  (point-max))
            (haskell-process-set-sent-stdin process t)
            (haskell-process-send-string process string)))
         ;; Otherwise we start a normal evaluation call.
         (t (setq haskell-interactive-mode-old-prompt-start
                  (copy-marker haskell-interactive-mode-prompt-start))
            (set-marker haskell-interactive-mode-prompt-start (point-max))
            (haskell-interactive-mode-history-add expr)
            (haskell-interactive-mode-do-expr expr)))))))

(defun haskell-interactive-mode-return ()
  "Handle the return key."
  (interactive)
  (cond
   ((haskell-interactive-at-compile-message)
    (next-error-internal))
   (t
    (haskell-interactive-handle-expr))))

(defun haskell-interactive-popup-error (response)
  "Popup an error."
  (if haskell-interactive-popup-errors
      (let ((buf (get-buffer-create "*HS-Error*")))
        (pop-to-buffer buf nil t)
        (with-current-buffer buf

          (haskell-error-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize response
                                'font-lock-face
                                'haskell-interactive-face-compile-error))
            (goto-char (point-min))
            (delete-blank-lines)
            (insert (propertize "-- Hit `q' to close this window.\n\n"
                                'font-lock-face 'font-lock-comment-face))
            (save-excursion
              (goto-char (point-max))
              (insert (propertize "\n-- To disable popups, customize `haskell-interactive-popup-errors'.\n\n"
                                  'font-lock-face 'font-lock-comment-face))))))
    (haskell-interactive-mode-insert-error response)))

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

(defun haskell-interactive-mode-clear ()
  "Clear the screen and put any current input into the history."
  (interactive)
  (let ((session (haskell-session)))
    (with-current-buffer (haskell-session-interactive-buffer session)
      (let ((inhibit-read-only t))
        (set-text-properties (point-min) (point-max) nil))
      (delete-region (point-min) (point-max))
      (remove-overlays)
      (haskell-interactive-mode-prompt session)
      (haskell-session-set session 'next-error-region nil)
      (haskell-session-set session 'next-error-locus nil))
    (with-current-buffer (get-buffer-create "*haskell-process-log*")
      (delete-region (point-min) (point-max))
      (remove-overlays))))

(defun haskell-interactive-mode-set-prompt (p)
  "Set (and overwrite) the current prompt."
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (goto-char haskell-interactive-mode-prompt-start)
    (delete-region (point) (point-max))
    (insert p)))

(defun haskell-interactive-mode-history-toggle (n)
  "Toggle the history n items up or down."
  (unless (null haskell-interactive-mode-history)
    (setq haskell-interactive-mode-history-index
          (mod (+ haskell-interactive-mode-history-index n)
               (length haskell-interactive-mode-history)))
    (unless (zerop haskell-interactive-mode-history-index)
      (message "History item: %d" haskell-interactive-mode-history-index))
    (haskell-interactive-mode-set-prompt
     (nth haskell-interactive-mode-history-index
          haskell-interactive-mode-history))))

(defun haskell-interactive-mode-history-previous (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (when (haskell-interactive-at-prompt)
    (if (not (zerop arg))
        (haskell-interactive-mode-history-toggle arg)
      (setq haskell-interactive-mode-history-index 0)
      (haskell-interactive-mode-history-toggle 1))))

(defun haskell-interactive-mode-history-next (arg)
  "Cycle forward through input history."
  (interactive "*p")
  (when (haskell-interactive-at-prompt)
    (if (not (zerop arg))
        (haskell-interactive-mode-history-toggle (- arg))
      (setq haskell-interactive-mode-history-index 0)
      (haskell-interactive-mode-history-toggle -1))))

(defun haskell-interactive-mode-completion-at-point-function ()
  "Offer completions for partial expression between prompt and point"
  (when (haskell-interactive-at-prompt)
    (let* ((process (haskell-process))
           (session (haskell-session))
           (inp (haskell-interactive-mode-input-partial)))
      (if (string= inp (car-safe haskell-interactive-mode-completion-cache))
          (cdr haskell-interactive-mode-completion-cache)
        (let* ((resp2 (haskell-process-get-repl-completions process inp))
               (rlen (-  (length inp) (length (car resp2))))
               (coll (append (if (string-prefix-p inp "import") '("import"))
                             (if (string-prefix-p inp "let") '("let"))
                             (cdr resp2)))
               (result (list (- (point) rlen) (point) coll)))
          (setq haskell-interactive-mode-completion-cache (cons inp result))
          result)))))

(defun haskell-interactive-next-error-function (&optional n reset)
  "See `next-error-function' for more information."

  (let* ((session (haskell-session))
         (next-error-region (haskell-session-get session 'next-error-region))
         (next-error-locus (haskell-session-get session 'next-error-locus))
         (reset-locus nil))

    (when (and next-error-region (or reset (and (/= n 0) (not next-error-locus))))
      (goto-char (car next-error-region))
      (unless (looking-at haskell-interactive-mode-error-regexp)
        (haskell-interactive-mode-error-forward))

      (setq reset-locus t)
      (unless (looking-at haskell-interactive-mode-error-regexp)
        (error "no errors found")))

    ;; move point if needed
    (cond
     (reset-locus nil)
     ((> n 0) (unless (haskell-interactive-mode-error-forward n)
                (error "no more errors")))

     ((< n 0) (unless (haskell-interactive-mode-error-backward (- n))
                (error "no more errors"))))

    (let ((orig-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

      (when (string-match haskell-interactive-mode-error-regexp orig-line)
        (let* ((msgmrk (set-marker (make-marker) (line-beginning-position)))
               (location (haskell-process-parse-error orig-line))
               (file (plist-get location :file))
               (line (plist-get location :line))
               (col1 (plist-get location :col))
               (col2 (plist-get location :col2))

               (cabal-relative-file (expand-file-name file (haskell-session-cabal-dir session)))
               (src-relative-file (expand-file-name file (haskell-session-current-dir session)))

               (real-file (cond ((file-exists-p cabal-relative-file) cabal-relative-file)
                                ((file-exists-p src-relative-file) src-relative-file))))

          (haskell-session-set session 'next-error-locus msgmrk)

          (if real-file
              (let ((m1 (make-marker))
                    (m2 (make-marker)))
                (with-current-buffer (find-file-noselect real-file)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (set-marker m1 (+ col1 (point) -1))

                    (when col2
                      (set-marker m2 (- (point) col2)))))
                ;; ...finally select&hilight error locus
                (compilation-goto-locus msgmrk m1 (and (marker-position m2) m2)))
            (error "don't know where to find %S" file)))))))

(defun haskell-interactive-mode-do-presentation (expr)
  "Present the given expression. Requires the `present` package
  to be installed. Will automatically import it qualified as Present."
  (let ((p (haskell-process)))
    ;; If Present.code isn't available, we probably need to run the
    ;; setup.
    (unless (string-match "^Present" (haskell-process-queue-sync-request p ":t Present.encode"))
      (haskell-interactive-mode-setup-presentation p))
    ;; Happily, let statements don't affect the `it' binding in any
    ;; way, so we can fake it, no pun intended.
    (let ((error (haskell-process-queue-sync-request
                  p (concat "let it = Present.asData (" expr ")"))))
      (if (not (string= "" error))
          (haskell-interactive-mode-eval-result (haskell-session) (concat error "\n"))
        (let ((hash (haskell-interactive-mode-presentation-hash)))
          (haskell-process-queue-sync-request
           p (format "let %s = Present.asData (%s)" hash expr))
          (let* ((presentation (haskell-interactive-mode-present-id
                                hash
                                (list 0))))
            (insert "\n")
            (haskell-interactive-mode-insert-presentation hash presentation)
            (haskell-interactive-mode-eval-result (haskell-session) "\n"))))
      (haskell-interactive-mode-prompt (haskell-session)))))

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
(defun haskell-session-all-modules (&optional dontcreate)
  "Get all modules -- installed or in the current project.
If DONTCREATE is non-nil don't create a new session."
  (append (haskell-session-installed-modules dontcreate)
          (haskell-session-project-modules dontcreate)))

(defun haskell-session-change ()
  "Change the session for the current buffer."
  (interactive)
  (haskell-session-assign (or (haskell-session-new-assume-from-cabal)
                              (haskell-session-choose)
                              (haskell-session-new))))

(defun haskell-session-project-modules (&optional dontcreate)
  "Get the modules of the current project.
If DONTCREATE is non-nil don't create a new session."
  (if (or (not dontcreate) (haskell-session-maybe))
      (let* ((session (haskell-session))
             (modules
              (shell-command-to-string
               (format "%s && %s"
                       (format "cd %s" (haskell-session-cabal-dir session))
                       ;; TODO: Use a different, better source. Possibly hasktags or some such.
                       ;; TODO: At least make it cross-platform. Linux
                       ;; (and possibly OS X) have egrep, Windows
                       ;; doesn't -- or does it via Cygwin or MinGW?
                       ;; This also doesn't handle module\nName. But those gits can just cut it out!
                       "egrep '^module[\t\r ]+[^(\t\r ]+' . -r -I --include='*.*hs' --include='*.hsc' -s -o -h | sed 's/^module[\t\r ]*//' | sort | uniq"))))
        (split-string modules))))

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

(defun haskell-mode-jump-to-def (ident)
  "Jump to definition of identifier at point."
  (interactive (list (haskell-ident-at-point)))
  (let ((loc (haskell-mode-find-def ident)))
    (when loc
      (haskell-mode-handle-generic-loc loc))))

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

(defun haskell-process-clear ()
  "Clear the current process."
  (interactive)
  (haskell-process-reset (haskell-process))
  (haskell-process-set (haskell-process) 'command-queue nil))

(defun haskell-process-interrupt ()
  "Interrupt the process (SIGINT)."
  (interactive)
  (interrupt-process (haskell-process-process (haskell-process))))

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

(defun haskell-process-cabal-macros ()
  "Send the cabal macros string."
  (interactive)
  (haskell-process-queue-without-filters (haskell-process)
                                         ":set -optP-include -optPdist/build/autogen/cabal_macros.h"))

(defun haskell-interactive-mode-present-id (hash id)
  "Generate a presentation for the current expression at ID."
  ;; See below for commentary of this statement.
  (let ((p (haskell-process)))
    (haskell-process-queue-without-filters
     p "let _it = it")
    (let* ((text (haskell-process-queue-sync-request
                  p
                  (format "Present.putStr (Present.encode (Present.fromJust (Present.present (Present.fromJust (Present.fromList [%s])) %s)))"
                          (mapconcat 'identity (mapcar 'number-to-string id) ",")
                          hash)))
           (reply
            (if (string-match "^*** " text)
                '((rep nil))
              (read text))))
      ;; Not necessary, but nice to restore it to the expression that
      ;; the user actually typed in.
      (haskell-process-queue-without-filters
       p "let it = _it")
      reply)))

(defun haskell-presentation-present-slot (btn)
  "The callback to evaluate the slot and present it in place of the button."
  (let ((id (button-get btn 'presentation-id))
        (hash (button-get btn 'hash))
        (parent-rep (button-get btn 'parent-rep))
        (continuation (button-get btn 'continuation)))
    (let ((point (point)))
      (button-put btn 'invisible t)
      (delete-region (button-start btn) (button-end btn))
      (haskell-interactive-mode-insert-presentation
       hash
       (haskell-interactive-mode-present-id hash id)
       parent-rep
       continuation)
      (when (> (point) point)
        (goto-char (1+ point))))))

(defun haskell-interactive-mode-presentation-slot (hash slot parent-rep &optional continuation)
  "Make a slot at point, pointing to ID."
  (let ((type (car slot))
        (id (cadr slot)))
    (if (member (intern type) '(Integer Char Int Float Double))
        (haskell-interactive-mode-insert-presentation
         hash
         (haskell-interactive-mode-present-id hash id)
         parent-rep
         continuation)
      (haskell-interactive-mode-presentation-slot-button slot parent-rep continuation hash))))

(defun haskell-interactive-mode-presentation-slot-button (slot parent-rep continuation hash)
  (let ((start (point))
        (type (car slot))
        (id (cadr slot)))
    (insert (propertize type 'font-lock-face '(:height 0.8 :underline t :inherit font-lock-comment-face)))
    (let ((button (make-text-button start (point)
                                    :type 'haskell-presentation-slot-button)))
      (button-put button 'hide-on-click t)
      (button-put button 'presentation-id id)
      (button-put button 'parent-rep parent-rep)
      (button-put button 'continuation continuation)
      (button-put button 'hash hash))))

(defun haskell-interactive-mode-insert-presentation (hash presentation &optional parent-rep continuation)
  "Insert the presentation, hooking up buttons for each slot."
  (let* ((rep (cadr (assoc 'rep presentation)))
         (text (cadr (assoc 'text presentation)))
         (type (cadr (assoc 'type presentation)))
         (slots (cadr (assoc 'slots presentation)))
         (nullary (null slots)))
    (cond
     ((string= "integer" rep)
      (insert (propertize text 'font-lock-face 'font-lock-constant)))
     ((string= "floating" rep)
      (insert (propertize text 'font-lock-face 'font-lock-constant)))
     ((string= "char" rep)
      (insert (propertize
               (if (string= "string" parent-rep)
                   (replace-regexp-in-string "^'\\(.+\\)'$" "\\1" text)
                 text)
               'font-lock-face 'font-lock-string-face)))
     ((string= "tuple" rep)
      (insert "(")
      (let ((first t))
        (cl-loop for slot in slots
                 do (unless first (insert ","))
                 do (haskell-interactive-mode-presentation-slot hash slot rep)
                 do (setq first nil)))
      (insert ")"))
     ((string= "list" rep)
      (if (null slots)
          (if continuation
              (progn (delete-char -1)
                     (delete-indentation))
            (insert "[]"))
        (let ((i 0))
          (unless continuation
            (insert "["))
          (let ((start-column (current-column)))
            (cl-loop for slot in slots
                     do (haskell-interactive-mode-presentation-slot
                         hash
                         slot
                         rep
                         (= i (1- (length slots))))
                     do (when (not (= i (1- (length slots))))
                          (insert "\n")
                          (indent-to (1- start-column))
                          (insert ","))
                     do (setq i (1+ i))))
          (unless continuation
            (insert "]")))))
     ((string= "string" rep)
      (unless (string= "string" parent-rep)
        (insert (propertize "\"" 'font-lock-face 'font-lock-string-face)))
      (cl-loop for slot in slots
               do (haskell-interactive-mode-presentation-slot hash slot rep))
      (unless (string= "string" parent-rep)
        (insert (propertize "\"" 'font-lock-face 'font-lock-string-face))))
     ((string= "alg" rep)
      (when (and parent-rep
                 (not nullary)
                 (not (string= "list" parent-rep)))
        (insert "("))
      (let ((start-column (current-column)))
        (insert (propertize text 'font-lock-face 'font-lock-type-face))
        (cl-loop for slot in slots
                 do (insert "\n")
                 do (indent-to (+ 2 start-column))
                 do (haskell-interactive-mode-presentation-slot hash slot rep)))
      (when (and parent-rep
                 (not nullary)
                 (not (string= "list" parent-rep)))
        (insert ")")))
     ((eq rep nil)
      (insert (propertize "?" 'font-lock-face 'font-lock-warning)))
     (t
      (let ((err "Unable to present! This very likely means Emacs
is out of sync with the `present' package. You should make sure
they're both up to date, or report a bug."))
        (insert err)
        (error err))))))

(defun haskell-interactive-mode-setup-presentation (p)
  "Setup the GHCi REPL for using presentations.

Using asynchronous queued commands as opposed to sync at this
stage, as sync would freeze up the UI a bit, and we actually
don't care when the thing completes as long as it's soonish."
  ;; Import dependencies under Present.* namespace
  (haskell-process-queue-without-filters p "import qualified Data.Maybe as Present")
  (haskell-process-queue-without-filters p "import qualified Data.ByteString.Lazy as Present")
  (haskell-process-queue-without-filters p "import qualified Data.AttoLisp as Present")
  (haskell-process-queue-without-filters p "import qualified Present.ID as Present")
  (haskell-process-queue-without-filters p "import qualified Present as Present")
  ;; Make a dummy expression to avoid "Loading package" nonsense
  (haskell-process-queue-without-filters
   p "Present.present (Present.fromJust (Present.fromList [0])) ()"))

(defvar haskell-interactive-mode-presentation-hash 0
  "Counter for the hash.")

(defun haskell-interactive-mode-presentation-hash ()
  "Generate a presentation hash."
  (format "_present_%s"
          (setq haskell-interactive-mode-presentation-hash
                (1+ haskell-interactive-mode-presentation-hash))))

(define-button-type 'haskell-presentation-slot-button
  'action 'haskell-presentation-present-slot
  'follow-link t
  'help-echo "Click to expand…")

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
;;;###autoload
(defun haskell-session-installed-modules (&optional dontcreate)
  "Get the modules installed in the current package set.
If DONTCREATE is non-nil don't create a new session."
  ;; TODO: Again, this makes HEAVY use of unix utilities. It'll work
  ;; fine in Linux, probably okay on OS X, and probably not at all on
  ;; Windows. Again, if someone wants to test on Windows and come up
  ;; with alternatives that's OK.
  ;;
  ;; Ideally all these package queries can be provided by a Haskell
  ;; program based on the Cabal API. Possibly as a nice service. Such
  ;; a service could cache and do nice things like that. For now, this
  ;; simple shell script takes us far.
  ;;
  ;; Probably also we can take the code from inferior-haskell-mode.
  ;;
  ;; Ugliness aside, if it saves us time to type it's a winner.
  ;;
  ;; FIXME/TODO: add support for (eq 'cabal-repl (haskell-process-type))
  (let ((modules (shell-command-to-string
                  (format "%s | %s | %s"
                          (if (eq 'cabal-dev (haskell-process-type))
                              (if (or (not dontcreate) (haskell-session-maybe))
                                  (format "cabal-dev -s %s/cabal-dev ghc-pkg dump"
                                          (haskell-session-cabal-dir (haskell-session)))
                                "echo ''")
                            "ghc-pkg dump")
                          "egrep '^(exposed-modules: |                 )[A-Z]'"
                          "cut -c18-"))))
    (split-string modules)))

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

(defun haskell-interactive-mode-eval-as-mode (session text)
  "Insert TEXT font-locked according to `haskell-interactive-mode-eval-mode'."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (let ((inhibit-read-only t))
      (delete-region (1+ haskell-interactive-mode-prompt-start) (point))
      (goto-char (point-max))
      (let ((start (point)))
        (insert (haskell-fontify-as-mode text
                                         haskell-interactive-mode-eval-mode))
        (when haskell-interactive-mode-collapse
          (haskell-collapse start (point)))))))

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

(defun haskell-interactive-mode-expr-result (state response)
  "Print the result of evaluating the expression."
  (let ((response
         (with-temp-buffer
           (insert (haskell-interactive-mode-cleanup-response
                    (cl-caddr state) response))
           (haskell-interactive-mode-handle-h (point-min))
           (buffer-string))))
    (when haskell-interactive-mode-eval-mode
      (unless (haskell-process-sent-stdin-p (cadr state))
        (haskell-interactive-mode-eval-as-mode (car state) response))))
  (haskell-interactive-mode-prompt (car state)))

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

(defun haskell-debug-buffer-name (session)
  "The debug buffer name for the current session."
  (format "*debug:%s*"
          (haskell-session-name session)))

(defmacro haskell-debug-with-breakpoints (&rest body)
  "Breakpoints need to exist to start stepping."
  `(if (haskell-debug-get-breakpoints)
       ,@body
     (error "No breakpoints to step into!")))

(defmacro haskell-debug-with-modules (&rest body)
  "Modules need to exist to do debugging stuff."
  `(if (haskell-debug-get-modules)
       ,@body
     (error "No modules loaded!")))

(defgroup haskell-debug nil
  "Settings for debugging support."
  :link '(custom-manual "(haskell-mode)haskell-debug")
  :group 'haskell)

(defface haskell-debug-warning-face
  '((t :inherit 'compilation-warning))
  "Face for warnings."
  :group 'haskell-debug)

(defface haskell-debug-trace-number-face
  '((t :weight bold :background "#f5f5f5"))
  "Face for numbers in backtrace."
  :group 'haskell-debug)

(defface haskell-debug-newline-face
  '((t :weight bold :background "#f0f0f0"))
  "Face for newlines in trace steps."
  :group 'haskell-debug)

(defface haskell-debug-keybinding-face
  '((t :inherit 'font-lock-type-face :weight bold))
  "Face for keybindings."
  :group 'haskell-debug)

(defface haskell-debug-heading-face
  '((t :inherit 'font-lock-keyword-face))
  "Face for headings."
  :group 'haskell-debug)

(defface haskell-debug-muted-face
  '((t :foreground "#999"))
  "Face for muteds."
  :group 'haskell-debug)

(defun haskell-debug-session-debugging-p (session)
  "Does the session have a debugging buffer open?"
  (not (not (get-buffer (haskell-debug-buffer-name session)))))

(defun haskell-debug-make-fake-history (context)
  "Make a fake history item."
  (list :index -1
        :path (plist-get context :path)
        :span (plist-get context :span)))

(defun haskell-debug-get-span-string (path span)
  "Get the string from the PATH and the SPAN."
  (save-window-excursion
    (find-file path)
    (buffer-substring
     (save-excursion
       (goto-char (point-min))
       (forward-line (1- (plist-get span :start-line)))
       (forward-char (1- (plist-get span :start-col)))
       (point))
     (save-excursion
       (goto-char (point-min))
       (forward-line (1- (plist-get span :end-line)))
       (forward-char (plist-get span :end-col))
       (point)))))

(defun haskell-debug-insert-bindings (modules breakpoints context)
  "Insert a list of bindings."
  (if breakpoints
      (progn (haskell-debug-insert-binding "s" "step into an expression")
             (haskell-debug-insert-binding "b" "breakpoint" t))
    (progn
      (when modules
        (haskell-debug-insert-binding "b" "breakpoint"))
      (when breakpoints
        (haskell-debug-insert-binding "s" "step into an expression" t))))
  (when breakpoints
    (haskell-debug-insert-binding "d" "delete breakpoint"))
  (when context
    (haskell-debug-insert-binding "a" "abandon context")
    (haskell-debug-insert-binding "c" "continue" t))
  (when context
    (haskell-debug-insert-binding "p" "previous step")
    (haskell-debug-insert-binding "n" "next step" t))
  (haskell-debug-insert-binding "g" "refresh" t)
  (insert "\n"))

(defun haskell-debug-insert-binding (binding desc &optional end)
  "Insert a helpful keybinding."
  (insert (propertize binding 'face 'haskell-debug-keybinding-face)
          (haskell-debug-muted " - ")
          desc
          (if end
              "\n"
            (haskell-debug-muted ", "))))

(defun haskell-debug/select ()
  "Select whatever is at point."
  (interactive)
  (cond
   ((get-text-property (point) 'break)
    (let ((break (get-text-property (point) 'break)))
      (haskell-debug-highlight (plist-get break :path)
                               (plist-get break :span))))
   ((get-text-property (point) 'module)
    (let ((break (get-text-property (point) 'module)))
      (haskell-debug-highlight (plist-get break :path))))))

(defun haskell-debug-highlight (path &optional span)
  "Highlight the file at span."
  (let ((p (make-overlay
            (line-beginning-position)
            (line-end-position))))
    (overlay-put p 'face `((:background "#eee")))
    (with-current-buffer
        (if span
            (save-window-excursion
              (find-file path)
              (current-buffer))
          (find-file path)
          (current-buffer))
      (let ((o (when span
                 (make-overlay
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- (plist-get span :start-line)))
                    (forward-char (1- (plist-get span :start-col)))
                    (point))
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- (plist-get span :end-line)))
                    (forward-char (plist-get span :end-col))
                    (point))))))
        (when o
          (overlay-put o 'face `((:background "#eee"))))
        (sit-for 0.5)
        (when o
          (delete-overlay o))
        (delete-overlay p)))))

(defun haskell-debug-insert-modules (modules)
  "Insert the list of modules."
  (haskell-debug-insert-header "Modules")
  (if (null modules)
      (haskell-debug-insert-muted "No loaded modules.")
    (progn (cl-loop for module in modules
                    do (insert (propertize (plist-get module :module)
                                           'module module
                                           'face `((:weight bold)))
                               (haskell-debug-muted " - ")
                               (propertize (file-name-nondirectory (plist-get module :path))
                                           'module module))
                    do (insert "\n")))))

(defun haskell-debug-insert-header (title)
  "Insert a header title."
  (insert (propertize title
                      'face 'haskell-debug-heading-face)
          "\n\n"))

(defun haskell-debug-insert-breakpoints (breakpoints)
  "Insert the list of breakpoints."
  (haskell-debug-insert-header "Breakpoints")
  (if (null breakpoints)
      (haskell-debug-insert-muted "No active breakpoints.")
    (cl-loop for break in breakpoints
             do (insert (propertize (format "%d"
                                            (plist-get break :number))
                                    'face `((:weight bold))
                                    'break break)
                        (haskell-debug-muted " - ")
                        (propertize (plist-get break :module)
                                    'break break
                                    'break break)
                        (haskell-debug-muted
                         (format " (%d:%d)"
                                 (plist-get (plist-get break :span) :start-line)
                                 (plist-get (plist-get break :span) :start-col)))
                        "\n")))
  (insert "\n"))

(defun haskell-debug-insert-muted (text)
  "Insert some muted text."
  (insert (haskell-debug-muted text)
          "\n"))

(defun haskell-debug-muted (text)
  "Make some muted text."
  (propertize text 'face 'haskell-debug-muted-face))

(defun haskell-debug-split-string (string)
  "Split GHCi's line-based output, stripping the trailing newline."
  (split-string string "\n" t))

(defun haskell-debug-parse-logged (string)
  "Parse the logged breakpoint."
  (cond
   ((string= "no more logged breakpoints\n" string)
    nil)
   ((string= "already at the beginning of the history\n" string)
    nil)
   (t
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (list :path (progn (search-forward " at ")
                         (buffer-substring-no-properties
                          (point)
                          (1- (search-forward ":"))))
            :span (haskell-debug-parse-span
                   (buffer-substring-no-properties
                    (point)
                    (line-end-position)))
            :types (progn (forward-line)
                          (haskell-debug-split-string
                           (buffer-substring-no-properties
                            (point)
                            (point-max)))))))))

(defun haskell-debug-parse-history-entry (string)
  "Parse a history entry."
  (if (string-match "^\\([-0-9]+\\)[ ]+:[ ]+\\([A-Za-z0-9_':]+\\)[ ]+(\\([^:]+\\):\\(.+?\\))$"
                    string)
      (list :index (string-to-number (match-string 1 string))
            :name (match-string 2 string)
            :path (match-string 3 string)
            :span (haskell-debug-parse-span (match-string 4 string)))
    (error "Unable to parse history entry: %s" string)))

(defun haskell-debug-parse-context (string)
  "Parse the context."
  (cond
   ((string-match "^--> \\(.+\\)\n  \\(.+\\)" string)
    (let ((name (match-string 1 string))
          (stopped (haskell-debug-parse-stopped-at (match-string 2 string))))
      (list :name name
            :path (plist-get stopped :path)
            :span (plist-get stopped :span))))))

(defun haskell-debug-parse-stopped-at (string)
  "Parse the location stopped at from the given string.

For example:

Stopped at /home/foo/project/src/x.hs:6:25-36

"
  (let ((index (string-match "Stopped at \\([^:]+\\):\\(.+\\)\n?"
                             string)))
    (when index
      (list :path (match-string 1 string)
            :span (haskell-debug-parse-span (match-string 2 string))
            :types (cdr (haskell-debug-split-string (substring string index)))))))

(defun haskell-debug-parse-module (string)
  "Parse a module and path.

For example:

X                ( /home/foo/X.hs, interpreted )

"
  (if (string-match "^\\([^ ]+\\)[ ]+( \\([^ ]+?\\), [a-z]+ )$"
                    string)
      (list :module (match-string 1 string)
            :path (match-string 2 string))
    (error "Unable to parse module from string: %s"
           string)))

(defun haskell-debug-parse-break-point (string)
  "Parse a breakpoint number, module and location from a string.

For example:

[13] Main /home/foo/src/x.hs:(5,1)-(6,37)

"
  (if (string-match "^\\[\\([0-9]+\\)\\] \\([^ ]+\\) \\([^:]+\\):\\(.+\\)$"
                    string)
      (list :number (string-to-number (match-string 1 string))
            :module (match-string 2 string)
            :path (match-string 3 string)
            :span (haskell-debug-parse-span (match-string 4 string)))
    (error "Unable to parse breakpoint from string: %s"
           string)))

(defun haskell-debug-parse-span (string)
  "Parse a source span from a string.

Examples:

  (5,1)-(6,37)
  6:25-36
  5:20

People like to make other people's lives interesting by making
variances in source span notation."
  (cond
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 1 string))
          :end-col (string-to-number (match-string 3 string))))
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 1 string))
          :end-col (string-to-number (match-string 2 string))))
   ((string-match "(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 3 string))
          :end-col (string-to-number (match-string 4 string))))
   (t (error "Unable to parse source span from string: %s"
             string))))

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
           (let ((module (haskell-complete-module-read "Module: " (haskell-session-all-modules))))
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

(provide 'haskell)
