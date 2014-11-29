;;; haskell-interactive-mode.el --- The interactive Haskell mode

;; Copyright (C) 2011-2012  Chris Done

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

;;; Commentary:

;;; Todo:

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'haskell-mode)
(require 'haskell-compile)
(require 'haskell-presentation-mode)
(require 'haskell-navigate-imports)
(require 'haskell-process)
(require 'haskell-collapse)
(require 'haskell-session)
(require 'haskell-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defcustom haskell-interactive-mode-scroll-to-bottom
  nil
  "Scroll to bottom in the REPL always."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-popup-errors
  t
  "Popup errors in a separate buffer."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-collapse
  nil
  "Collapse printed results."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-types-for-show-ambiguous
  t
  "Show types when there's no Show instance or there's an
ambiguous class constraint."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-eval-pretty
  nil
  "Print eval results that can be parsed as Show instances prettily. Requires sexp-show (on Hackage)."
  :type 'boolean
  :group 'haskell-interactive)

(defvar haskell-interactive-prompt "λ> "
  "The prompt to use.")

(defun haskell-interactive-prompt-regex ()
  "Generate a regex for searching for any occurence of the prompt
at the beginning of the line. This should prevent any
interference with prompts that look like haskell expressions."
  (concat "^" (regexp-quote haskell-interactive-prompt)))

(defvar haskell-interactive-mode-prompt-start
  nil
  "Mark used for the beginning of the prompt.")

(defvar haskell-interactive-mode-result-end
  nil
  "Mark used to figure out where the end of the current result
  output is. Used to distinguish betwen user input.")

(defvar haskell-interactive-mode-old-prompt-start
  nil
  "Mark used for the old beginning of the prompt.")

(defcustom haskell-interactive-mode-eval-mode
  nil
  "Use the given mode's font-locking to render some text."
  :type '(choice function (const :tag "None" nil))
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-hide-multi-line-errors
  nil
  "Hide collapsible multi-line compile messages by default."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-delete-superseded-errors
  t
  "Whether to delete compile messages superseded by recompile/reloads."
  :type 'boolean
  :group 'haskell-interactive)

(defcustom haskell-interactive-mode-include-file-name
  t
  "Include the file name of the module being compiled when
printing compilation messages."
  :type 'boolean
  :group 'haskell-interactive)

(add-hook 'haskell-process-ended-hook 'haskell-process-prompt-restart)

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

;; buffer-local variables used internally by `haskell-interactive-mode'
(defvar haskell-interactive-mode-history)
(defvar haskell-interactive-mode-history-index)
(defvar haskell-interactive-mode-completion-cache)

;;;###autoload
(define-derived-mode haskell-interactive-mode fundamental-mode "Interactive-Haskell"
  "Interactive mode for Haskell.

See Info node `(haskell-mode)haskell-interactive-mode' for more
information.

Key bindings:
\\{haskell-interactive-mode-map}"
  :group 'haskell-interactive
  (set (make-local-variable 'haskell-interactive-mode-history) (list))
  (set (make-local-variable 'haskell-interactive-mode-history-index) 0)
  (set (make-local-variable 'haskell-interactive-mode-completion-cache) nil)

  (setq next-error-function 'haskell-interactive-next-error-function)
  (add-hook 'completion-at-point-functions
            'haskell-interactive-mode-completion-at-point-function nil t)

  (haskell-interactive-mode-prompt))

(defface haskell-interactive-face-prompt
  '((t :inherit font-lock-function-name-face))
  "Face for the prompt."
  :group 'haskell-interactive)

(defface haskell-interactive-face-compile-error
  '((t :inherit compilation-error))
  "Face for compile errors."
  :group 'haskell-interactive)

(defface haskell-interactive-face-compile-warning
  '((t :inherit compilation-warning))
  "Face for compiler warnings."
  :group 'haskell-interactive)

(defface haskell-interactive-face-result
  '((t :inherit font-lock-string-face))
  "Face for the result."
  :group 'haskell-interactive)

(defface haskell-interactive-face-garbage
  '((t :inherit font-lock-string-face))
  "Face for trailing garbage after a command has completed."
  :group 'haskell-interactive)

(defun haskell-interactive-mode-newline-indent ()
  "Make newline and indent."
  (interactive)
  (newline)
  (indent-according-to-mode))

(defun haskell-interactive-mode-kill-whole-line ()
  "Kill the whole REPL line."
  (interactive)
  (kill-region haskell-interactive-mode-prompt-start
               (line-end-position)))

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

(defvar haskell-interactive-previous-buffer nil
  "Records the buffer to which `haskell-interactive-switch-back' should jump.
This is set by `haskell-interactive-switch', and should otherwise
be nil.")
(make-variable-buffer-local 'haskell-interactive-previous-buffer)

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

(defun haskell-interactive-switch-back ()
  "Switch back to the buffer from which this interactive buffer was reached."
  (interactive)
  (if haskell-interactive-previous-buffer
      (switch-to-buffer-other-window haskell-interactive-previous-buffer)
    (message "No previous buffer.")))

(defun haskell-interactive-mode-return ()
  "Handle the return key."
  (interactive)
  (cond
   ((haskell-interactive-at-compile-message)
    (next-error-internal))
   (t
    (haskell-interactive-handle-expr))))

(defun haskell-interactive-mode-space (n)
  "Handle the space key."
  (interactive "p")
  (if (and (bound-and-true-p god-local-mode)
           (fboundp 'god-mode-self-insert))
      (call-interactively 'god-mode-self-insert)
    (if (haskell-interactive-at-compile-message)
        (next-error-no-select 0)
      (self-insert-command n))))

(defun haskell-interactive-at-prompt ()
  "If at prompt, returns start position of user-input, otherwise returns nil."
  (if (>= (point)
          haskell-interactive-mode-prompt-start)
      haskell-interactive-mode-prompt-start
    nil))

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

(defun haskell-interactive-mode-do-expr (expr)
  (cond
   ((string-match "^:present " expr)
    (haskell-interactive-mode-do-presentation (replace-regexp-in-string "^:present " "" expr)))
   (t
    (haskell-interactive-mode-run-expr expr))))

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

(defun haskell-interactive-mode-insert-error (response)
  "Insert an error message."
  (insert "\n"
          (haskell-fontify-as-mode
           response
           'haskell-mode))
  (haskell-interactive-mode-prompt))

(define-derived-mode haskell-error-mode
  special-mode "Error"
  "Major mode for viewing Haskell compile errors.")

;; (define-key haskell-error-mode-map (kbd "q") 'quit-window)

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

(defun haskell-interactive-mode-handle-h (&optional bound)
  "Handle ^H in output."
  (let ((bound (point-min))
        (inhibit-read-only t))
    (save-excursion
      (while (search-backward "\b" bound t 1)
        (save-excursion
          (forward-char)
          (let ((end (point)))
            (if (search-backward-regexp "[^\b]" bound t 1)
                (forward-char)
              (goto-char (point-min)))
            (let ((start (point)))
              (delete-region (max (- (point) (- end start))
                                  (point-min))
                             end))))))))

(defun haskell-interactive-mode-cleanup-response (expr response)
  "Ignore the mess that GHCi outputs on multi-line input."
  (if (not (string-match "\n" expr))
      response
    (let ((i 0)
          (out "")
          (lines (length (split-string expr "\n"))))
      (cl-loop for part in (split-string response "| ")
               do (setq out
                        (concat out
                                (if (> i lines)
                                    (concat (if (or (= i 0) (= i (1+ lines))) "" "| ") part)
                                  "")))
               do (setq i (1+ i)))
      out)))

(defun haskell-interactive-mode-multi-line (expr)
  "If a multi-line expression has been entered, then reformat it to be:

:{
do the
   multi-liner
   expr
:}
"
  (if (not (string-match "\n" expr))
      expr
    (let* ((i 0)
           (lines (split-string expr "\n"))
           (len (length lines))
           (indent (make-string (length haskell-interactive-prompt)
                                ? )))
      (mapconcat 'identity
                 (cl-loop for line in lines
                          collect (cond ((= i 0)
                                         (concat ":{" "\n" line))
                                        ((= i (1- len))
                                         (concat line "\n" ":}"))
                                        (t
                                         line))
                          do (setq i (1+ i)))
                 "\n"))))

(defun haskell-interactive-trim (line)
  "Trim indentation off of lines in the REPL."
  (if (and (string-match "^[ ]+" line)
           (> (length line)
              (length haskell-interactive-prompt)))
      (substring line
                 (length haskell-interactive-prompt))
    line))

(defun haskell-interactive-mode-line-is-query (line)
  "Is LINE actually a :t/:k/:i?"
  (and (string-match "^:[itk] " line)
       t))

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

(defun haskell-interactive-mode-beginning ()
  "Go to the start of the line."
  (interactive)
  (if (haskell-interactive-at-prompt)
      (goto-char haskell-interactive-mode-prompt-start)
    (move-beginning-of-line nil)))

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

(defun haskell-interactive-mode-input-partial ()
  "Get the interactive mode input up to point."
  (let ((input-start (haskell-interactive-at-prompt)))
    (unless input-start
      (error "not at prompt"))
    (buffer-substring-no-properties input-start (point))))

(defun haskell-interactive-mode-input ()
  "Get the interactive mode input."
  (buffer-substring-no-properties
   haskell-interactive-mode-prompt-start
   (point-max)))

(defun haskell-interactive-mode-prompt (&optional session)
  "Show a prompt at the end of the REPL buffer.
If SESSION is non-nil, use the REPL buffer associated with
SESSION, otherwise operate on the current buffer.
"
  (with-current-buffer (if session
                           (haskell-session-interactive-buffer session)
                         (current-buffer))
    (goto-char (point-max))
    (insert (propertize haskell-interactive-prompt
                        'font-lock-face 'haskell-interactive-face-prompt
                        'read-only t
                        'rear-nonsticky t
                        'prompt t))
    (let ((marker (set (make-local-variable 'haskell-interactive-mode-prompt-start)
                       (make-marker))))
      (set-marker marker
                  (point)
                  (current-buffer))
      (when nil
        (let ((o (make-overlay (point) (point-max) nil nil t)))
          (overlay-put o 'line-prefix (make-string (length haskell-interactive-prompt)
                                                   ? )))))
    (when haskell-interactive-mode-scroll-to-bottom
      (haskell-interactive-mode-scroll-to-bottom))))

(defun haskell-interactive-mode-eval-result (session text)
  "Insert the result of an eval as plain text."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (goto-char (point-max))
    (let ((start (point)))
      (insert (ansi-color-apply
               (propertize text
                           'font-lock-face 'haskell-interactive-face-result
                           'rear-nonsticky t
                           'read-only t
                           'prompt t
                           'result t)))
      (haskell-interactive-mode-handle-h start))
    (let ((marker (set (make-local-variable 'haskell-interactive-mode-result-end)
                       (make-marker))))
      (set-marker marker
                  (point)
                  (current-buffer)))
    (when haskell-interactive-mode-scroll-to-bottom
      (haskell-interactive-mode-scroll-to-bottom))))

(defun haskell-interactive-mode-scroll-to-bottom ()
  "Scroll to bottom."
  (let ((w (get-buffer-window (current-buffer))))
    (when w
      (goto-char (point-max))
      (set-window-point w (point-max)))))

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

(defun haskell-interactive-mode-compile-error (session message)
  "Echo an error."
  (haskell-interactive-mode-compile-message
   session message 'haskell-interactive-face-compile-error))

(defun haskell-interactive-mode-compile-warning (session message)
  "Warning message."
  (haskell-interactive-mode-compile-message
   session message 'haskell-interactive-face-compile-warning))

(defun haskell-interactive-mode-compile-message (session message type)
  "Echo a compiler warning."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (setq next-error-last-buffer (current-buffer))
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (let ((lines (string-match "^\\(.*\\)\n\\([[:unibyte:][:nonascii:]]+\\)" message)))
        (when lines
          (insert (propertize (concat (match-string 1 message) " …\n")
                              'font-lock-face type
                              'read-only t
                              'rear-nonsticky t
                              'expandable t))
          (insert (propertize (concat (match-string 2 message) "\n")
                              'font-lock-face type
                              'read-only t
                              'rear-nonsticky t
                              'collapsible t
                              'invisible haskell-interactive-mode-hide-multi-line-errors
                              'message-length (length (match-string 2 message)))))
        (unless lines
          (insert (propertize (concat message "\n")
                              'font-lock-face type
                              'read-only t
                              'rear-nonsticky t)))))))

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

(defun haskell-interactive-mode-insert (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (propertize message
                          'read-only t
                          'rear-nonsticky t)))))

(defun haskell-interactive-mode-goto-end-point ()
  "Go to the 'end' of the buffer (before the prompt.)"
  (goto-char haskell-interactive-mode-prompt-start)
  (goto-char (line-beginning-position)))

(defun haskell-interactive-mode-history-add (input)
  "Add item to the history."
  (setq haskell-interactive-mode-history
        (cons ""
              (cons input
                    (cl-remove-if (lambda (i) (or (string= i input) (string= i "")))
                                  haskell-interactive-mode-history))))
  (setq haskell-interactive-mode-history-index
        0))

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

(defun haskell-interactive-mode-set-prompt (p)
  "Set (and overwrite) the current prompt."
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (goto-char haskell-interactive-mode-prompt-start)
    (delete-region (point) (point-max))
    (insert p)))

(defun haskell-interactive-buffer ()
  "Get the interactive buffer of the session."
  (haskell-session-interactive-buffer (haskell-session)))

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

(defun haskell-interactive-mode-tab ()
  "Do completion if at prompt or else try collapse/expand."
  (interactive)
  (cond
   ((haskell-interactive-at-prompt)
    (completion-at-point))
   ((get-text-property (point) 'collapsible)
    (let ((column (current-column)))
      (search-backward-regexp "^[^ ]")
      (haskell-interactive-mode-tab-expand)
      (goto-char (+ column (line-beginning-position)))))
   (t (haskell-interactive-mode-tab-expand))))

(defun haskell-interactive-mode-tab-expand ()
  "Expand the rest of the message."
  (cond ((get-text-property (point) 'expandable)
         (let* ((pos (1+ (line-end-position)))
                (visibility (get-text-property pos 'invisible))
                (length (1+ (get-text-property pos 'message-length))))
           (let ((inhibit-read-only t))
             (put-text-property pos
                                (+ pos length)
                                'invisible
                                (not visibility)))))))

(defconst haskell-interactive-mode-error-regexp
  "^\\([A-Z]?:?[^\r\n:]+\\):\\([0-9()-:]+\\):?")

(defun haskell-interactive-at-compile-message ()
  "Am I on a compile message?"
  (and (not (haskell-interactive-at-prompt))
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at haskell-interactive-mode-error-regexp))))

(defun haskell-interactive-mode-error-backward (&optional count)
  "Go backward to the previous error."
  (interactive)
  (search-backward-regexp haskell-interactive-mode-error-regexp nil t count))

(defun haskell-interactive-mode-error-forward (&optional count)
  "Go forward to the next error, or return to the REPL."
  (interactive)
  (goto-char (line-end-position))
  (if (search-forward-regexp haskell-interactive-mode-error-regexp nil t count)
      (progn (goto-char (line-beginning-position))
             t)
    (progn (goto-char (point-max))
           nil)))

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

(defun haskell-interactive-mode-delete-compile-messages (session &optional file-name)
  "Delete compile messages in REPL buffer.
If FILE-NAME is non-nil, restrict to removing messages concerning
FILE-NAME only."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^Compilation failed.$" nil t 1)
        (let ((inhibit-read-only t))
          (delete-region (line-beginning-position)
                         (1+ (line-end-position))))
        (goto-char (point-min)))
      (while (when (re-search-forward haskell-interactive-mode-error-regexp nil t)
               (let ((msg-file-name (match-string-no-properties 1))
                     (msg-startpos (line-beginning-position)))
                 ;; skip over hanging continuation message lines
                 (while (progn (forward-line) (looking-at "^[ ]+")))

                 (when (or (not file-name) (string= file-name msg-file-name))
                   (let ((inhibit-read-only t))
                     (set-text-properties msg-startpos (point) nil))
                   (delete-region msg-startpos (point))
                   ))
               t)))))

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
(defun haskell-interactive-mode-reset-error (session)
  "Reset the error cursor position."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer session)
    (haskell-interactive-mode-goto-end-point)
    (let ((mrk (point-marker)))
      (haskell-session-set session 'next-error-locus nil)
      (haskell-session-set session 'next-error-region (cons mrk (copy-marker mrk t))))
    (goto-char (point-max))))

(defun haskell-interactive-kill ()
  "Kill the buffer and (maybe) the session."
  (interactive)
  (when (eq major-mode 'haskell-interactive-mode)
    (when (and (boundp 'haskell-session)
               haskell-session
               (y-or-n-p "Kill the whole session?"))
      (haskell-session-kill t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Presentation

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(add-hook 'kill-buffer-hook 'haskell-interactive-kill)

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

(defun haskell-session-interactive-buffer (s)
  "Get the session interactive buffer."
  (let ((buffer (haskell-session-get s 'interactive-buffer)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (let ((buffer (get-buffer-create (format "*%s*" (haskell-session-name s)))))
        (haskell-session-set-interactive-buffer s buffer)
        (with-current-buffer buffer
          (haskell-interactive-mode)
          (haskell-session-assign s))
        (switch-to-buffer-other-window buffer)
        buffer))))

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

;;;###autoload
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

(defun haskell-process-restart ()
  "Restart the inferior Haskell process."
  (interactive)
  (haskell-process-reset (haskell-process))
  (haskell-process-set (haskell-process) 'command-queue nil)
  (haskell-process-start (haskell-session)))

(defun haskell-kill-session-process (&optional session)
  "Kill the process."
  (interactive)
  (let* ((session (or session (haskell-session)))
         (existing-process (get-process (haskell-session-name session))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Killing process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process))))

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

;;;###autoload
(defun haskell-process ()
  "Get the current process from the current session."
  (haskell-session-process (haskell-session)))

;;;###autoload
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

(defun haskell-mode-show-type-at (&optional insert-value)
  "Show the type of the thing at point."
  (interactive "P")
  (let ((ty (haskell-mode-type-at)))
    (if insert-value
        (progn (goto-char (line-beginning-position))
               (insert (haskell-fontify-as-mode ty 'haskell-mode)
                       "\n"))
      (message "%s" (haskell-fontify-as-mode ty 'haskell-mode)))))

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

(defun haskell-process-cabal-macros ()
  "Send the cabal macros string."
  (interactive)
  (haskell-process-queue-without-filters (haskell-process)
                                         ":set -optP-include -optPdist/build/autogen/cabal_macros.h"))

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

(defun haskell-process-cabal-live (state buffer)
  "Do live updates for Cabal processes."
  (haskell-interactive-mode-insert
   (haskell-process-session (cadr state))
   (replace-regexp-in-string
    haskell-process-prompt-regex
    ""
    (substring buffer (cl-cadddr state))))
  (setf (cl-cdddr state) (list (length buffer)))
  nil)

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

(defun haskell-process-parse-error (string)
  "Parse the line number from the error."
  (let ((span nil))
    (cl-loop for regex
             in haskell-compilation-error-regexp-alist
             do (when (string-match (car regex) string)
                  (setq span
                        (list :file (match-string 1 string)
                              :line (string-to-number (match-string 2 string))
                              :col (string-to-number (match-string 4 string))
                              :line2 (when (match-string 3 string)
                                       (string-to-number (match-string 3 string)))
                              :col2 (when (match-string 5 string)
                                      (string-to-number (match-string 5 string)))))))
    span))

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

(defun haskell-process-suggest-add-package (session msg)
  "Add the (matched) module to your cabal file."
  (let* ((suggested-package (match-string 1 msg))
         (package-name (replace-regexp-in-string "-[^-]+$" "" suggested-package))
         (version (progn (string-match "\\([^-]+\\)$" suggested-package)
                         (match-string 1 suggested-package)))
         (cabal-file (concat (haskell-session-name session)
                             ".cabal")))
    (when (y-or-n-p
           (format "Add `%s' to %s?"
                   package-name
                   cabal-file))
      (haskell-cabal-add-dependency package-name version nil t))))

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

(defun haskell-process-suggest-remove-import (session file import line)
  "Suggest removing or commenting out IMPORT on LINE."
  (let ((continue t)
        (first t))
    (cl-case (read-event
              (propertize (format "%sThe import line `%s' is redundant. Remove? (y, n, c: comment out)  "
                                  (if (not first)
                                      "Please answer n, y or c: "
                                    "")
                                  import)
                          'face 'minibuffer-prompt))
      (?y
       (haskell-process-find-file session file)
       (save-excursion
         (goto-char (point-min))
         (forward-line (1- line))
         (goto-char (line-beginning-position))
         (delete-region (line-beginning-position)
                        (line-end-position))))
      (?n
       (message "Ignoring redundant import %s" import))
      (?c
       (haskell-process-find-file session file)
       (save-excursion
         (goto-char (point-min))
         (forward-line (1- line))
         (goto-char (line-beginning-position))
         (insert "-- "))))))

(defun haskell-process-suggest-pragma (session pragma extension file)
  "Suggest to add something to the top of the file."
  (let ((string  (format "{-# %s %s #-}" pragma extension)))
    (when (y-or-n-p (format "Add %s to the top of the file? " string))
      (haskell-process-find-file session file)
      (save-excursion
        (goto-char (point-min))
        (insert (concat string "\n"))))))


(provide 'haskell-interactive-mode)

;;; haskell-interactive-mode.el ends here
