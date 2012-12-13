;;; haskell-interactive-mode.el -- The interactive Haskell mode.

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

(eval-when-compile (require 'cl))
(require 'haskell-show)

(defcustom haskell-interactive-mode-eval-pretty
  nil
  "Print eval results that can be parsed as Show instances prettily. Requires sexp-show (on Hackage)."
  :type 'boolean
  :group 'haskell)

(defvar haskell-interactive-prompt "λ> "
  "The prompt to use.")

(defcustom haskell-interactive-mode-eval-mode
  nil
  "Use the given mode's font-locking to render some text."
  :type '(choice function (const :tag "None" nil))
  :group 'haskell)

(defvar haskell-interactive-greetings
  (list "Hello, Haskell!"
        "The lambdas must flow."
        "Hours of hacking await!"
        "The next big Haskell project is about to start!"
        "Your wish is my IO ().")
  "Greetings for when the Haskell process starts up.")

;;;###autoload
(defun haskell-interactive-mode (session)
  "Interactive mode for Haskell.

Key bindings:
\\{haskell-interactive-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (haskell-session-assign session)
  (use-local-map haskell-interactive-mode-map)
  (set (make-local-variable 'haskell-interactive-mode) t)
  (setq major-mode 'haskell-interactive-mode)
  (setq mode-name "Interactive-Haskell")
  (run-mode-hooks 'haskell-interactive-mode-hook)
  (set (make-local-variable 'haskell-interactive-mode-history)
       (list))
  (set (make-local-variable 'haskell-interactive-mode-history-index)
       0)
  (haskell-interactive-mode-prompt session))

(defface haskell-interactive-face-prompt
  '((t :inherit 'font-lock-function-name-face))
  "Face for the prompt."
  :group 'haskell)

(defface haskell-interactive-face-compile-error
  '((t :inherit 'compilation-error))
  "Face for compile errors."
  :group 'haskell)

(defface haskell-interactive-face-compile-warning
  '((t :inherit 'compilation-warning))
  "Face for compiler warnings."
  :group 'haskell)

(defface haskell-interactive-face-result
  '((t :inherit 'font-lock-string-face))
  "Face for the result."
  :group 'haskell)

(defvar haskell-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'haskell-interactive-mode-return)
    (define-key map (kbd "C-j") 'haskell-interactive-mode-newline-indent)
    (define-key map (kbd "C-a") 'haskell-interactive-mode-beginning)
    (define-key map (kbd "<home>") 'haskell-interactive-mode-beginning)
    (define-key map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key map (kbd "C-c C-c") 'haskell-process-interrupt)
    (define-key map (kbd "M-p")
      '(lambda () (interactive) (haskell-interactive-mode-history-toggle 1)))
    (define-key map (kbd "M-n")
      '(lambda () (interactive) (haskell-interactive-mode-history-toggle -1)))
    (define-key map (kbd "TAB") 'haskell-interactive-mode-tab)
    map)
  "Interactive Haskell mode map.")

(defun haskell-interactive-mode-newline-indent ()
  "Make newline and indent."
  (interactive)
  (insert "\n" (make-string (length haskell-interactive-prompt) ? )))

;;;###autoload
(defun haskell-interactive-bring ()
  "Bring up the interactive mode for this session."
  (interactive)
  (let* ((session (haskell-session))
         (buffer (haskell-session-interactive-buffer session)))
    (unless (and (find-if (lambda (window) (equal (window-buffer window) buffer))
                          (window-list))
                 (= 2 (length (window-list))))
      (delete-other-windows)
      (display-buffer buffer)
      (other-window 1))))

;;;###autoload
(defun haskell-interactive-switch ()
  "Switch to the interactive mode for this session."
  (interactive)
  (let ((session (haskell-session)))
    (let ((buffer (haskell-session-interactive-buffer session)))
      (unless (find-if (lambda (window) (equal (window-buffer window) buffer))
                       (window-list))
        (switch-to-buffer-other-window (haskell-session-interactive-buffer session))))))

(defun haskell-interactive-mode-return ()
  "Handle the return key."
  (interactive)
  (or (haskell-interactive-jump-to-error-line)
      (haskell-interactive-handle-line)))

(defun haskell-interactive-at-prompt ()
  "Am I at the prompt?"
  (let ((current-point (point)))
    (save-excursion (goto-char (point-max))
                    (search-backward-regexp haskell-interactive-prompt)
                    (> current-point (point)))))

(defun haskell-interactive-handle-line ()
  (when (haskell-interactive-at-prompt)
    (let ((expr (haskell-interactive-mode-input))
          (session (haskell-session))
          (process (haskell-process)))
      (when (not (string= "" (replace-regexp-in-string " " "" expr)))
        (haskell-interactive-mode-history-add expr)
        (goto-char (point-max))
        (haskell-process-queue-command
         process
         (make-haskell-command
          :state (list session process expr 0)
          :go (lambda (state)
                (haskell-process-send-string (cadr state)
                                             (caddr state)))
          :live (lambda (state buffer)
                  (unless (string= ":q" (caddr state))
                    (let* ((cursor (cadddr state))
                           (next (replace-regexp-in-string
                                  haskell-process-prompt-regex
                                  "\n"
                                  (substring buffer cursor))))
                      (when (= 0 cursor) (insert "\n"))
                      (haskell-interactive-mode-eval-result (car state) next)
                      (setf (cdddr state) (list (length buffer)))
                      nil)))
          :complete (lambda (state response)
                      (if haskell-interactive-mode-eval-mode
                          (haskell-interactive-mode-eval-as-mode (car state) response)
                        (when haskell-interactive-mode-eval-pretty
                          (haskell-interactive-mode-eval-pretty-result (car state) response)))
                      (haskell-interactive-mode-prompt (car state)))))))))

(defun haskell-interactive-jump-to-error-line ()
  "Jump to the error line."
  (let ((orig-line (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))
    (and (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" orig-line)
         (let ((file (match-string 1 orig-line))
               (line (match-string 2 orig-line))
               (col (match-string 3 orig-line)))
           (let* ((session (haskell-session))
                  (cabal-path (haskell-session-cabal-dir session))
                  (src-path (haskell-session-current-dir session))
                  (cabal-relative-file (concat cabal-path "/" file))
                  (src-relative-file (concat src-path "/" file))
                  (cabal-relative-file-rel (concat cabal-path "/"
                                                   (file-relative-name file
                                                                       cabal-path)))
                  (src-relative-file-rel (concat src-path "/"
                                                 (file-relative-name file
                                                                     src-path))))
             (let ((file (cond ((file-exists-p cabal-relative-file)
                                cabal-relative-file)
                               ((file-exists-p src-relative-file)
                                src-relative-file)
                               ((file-exists-p src-relative-file-rel)
                                src-relative-file)
                               ((file-exists-p cabal-relative-file-rel)
                                cabal-relative-file))))
               (when file
                 (other-window 1)
                 (find-file file)
                 (haskell-interactive-bring)
                 (goto-char (point-min))
                 (forward-line (1- (string-to-number line)))
                 (goto-char (+ (point) (string-to-number col)))
                 (haskell-mode-message-line orig-line)
                 t)))))))

(defun haskell-interactive-mode-beginning ()
  "Go to the start of the line."
  (interactive)
  (if (search-backward-regexp haskell-interactive-prompt (line-beginning-position) t 1)
      (search-forward-regexp haskell-interactive-prompt (line-end-position) t 1)
    (move-beginning-of-line nil)))

(defun haskell-interactive-mode-clear ()
  "Newline and indent at the prompt."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (let ((inhibit-read-only t))
      (set-text-properties (point-min) (point-max) nil))
    (delete-region (point-min) (point-max))
    (mapc 'delete-overlay (overlays-in (point-min) (point-max)))
    (haskell-interactive-mode-prompt (haskell-session))))

(defun haskell-interactive-mode-input ()
  "Get the interactive mode input."
  (substring
   (buffer-substring-no-properties
    (save-excursion
      (goto-char (max (point-max)))
      (search-backward-regexp haskell-interactive-prompt))
    (line-end-position))
   (length haskell-interactive-prompt)))

(defun haskell-interactive-mode-prompt (session)
  "Show a prompt at the end of the buffer."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (goto-char (point-max))
    (insert (propertize haskell-interactive-prompt
                        'face 'haskell-interactive-face-prompt
                        'read-only t
                        'rear-nonsticky t
                        'prompt t))))

(defun haskell-interactive-mode-eval-result (session text)
  "Insert the result of an eval as plain text."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (goto-char (point-max))
    (insert (propertize text
                        'face 'haskell-interactive-face-result
                        'rear-nonsticky t
                        'read-only t
                        'prompt t
                        'result t))))

(defun haskell-interactive-mode-eval-as-mode (session text)
  "Insert the result of an eval as a pretty printed Showable, if
  parseable, or otherwise just as-is."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (let ((start-point (save-excursion (search-backward-regexp haskell-interactive-prompt)
                                       (forward-line 1)
                                       (point)))
          (inhibit-read-only t))
      (delete-region start-point (point))
      (goto-char (point-max))
      (insert (let ((mode haskell-interactive-mode-eval-mode))
                (with-current-buffer (get-buffer-create (concat "*print-" (symbol-name mode) "*"))
                  (unless (eq major-mode mode)
                    (funcall mode))
                  (erase-buffer)
                  (insert text)
                  (font-lock-fontify-region (point-min) (point-max))
                  (buffer-substring (point-min)
                                    (point-max)))))
      (insert "\n"))))

(defun haskell-interactive-mode-eval-pretty-result (session text)
  "Insert the result of an eval as a pretty printed Showable, if
  parseable, or otherwise just as-is."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (let ((start-point (save-excursion (search-backward-regexp haskell-interactive-prompt)
                                       (forward-line 1)
                                       (point)))
          (inhibit-read-only t))
      (delete-region start-point (point))
      (goto-char (point-max))
      (haskell-show-parse-and-insert text)
      (insert "\n"))))

;;;###autoload
(defun haskell-interactive-mode-echo (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (propertize (concat message "\n")
                          'read-only t
                          'rear-nonsticky t)))))

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
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (let ((lines (string-match "^\\(.*\\)\n\\([[:unibyte:][:nonascii:]]+\\)" message)))
        (when lines
          (insert (propertize (concat (match-string 1 message) " …\n")
                              'face type
                              'read-only t
                              'rear-nonsticky t
                              'expandable t))
          (insert (propertize (concat (match-string 2 message) "\n")
                              'face type
                              'read-only t
                              'rear-nonsticky t
                              'collapsible t
                              'invisible t
                              'message-length (length (match-string 2 message)))))
        (unless lines
          (insert (propertize (concat message "\n")
                              'face type
                              'read-only t
                              'rear-nonsticky t)))))))

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
  (goto-char (point-max))
  (when (search-backward-regexp haskell-interactive-prompt (point-min) t 1)))

(defun haskell-interactive-mode-history-add (input)
  "Add item to the history."
  (setq haskell-interactive-mode-history
        (cons ""
              (cons input
                    (remove-if (lambda (i) (or (string= i input) (string= i "")))
                               haskell-interactive-mode-history))))
  (setq haskell-interactive-mode-history-index
        0))

(defun haskell-interactive-mode-history-toggle (n)
  "Toggle the history n items up or down."
  (unless (null haskell-interactive-mode-history)
    (setq haskell-interactive-mode-history-index
          (mod (+ haskell-interactive-mode-history-index n)
               (length haskell-interactive-mode-history)))
    (haskell-interactive-mode-set-prompt
     (nth haskell-interactive-mode-history-index
          haskell-interactive-mode-history))))

(defun haskell-interactive-mode-set-prompt (p)
  "Set (and overwrite) the current prompt."
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (goto-char (point-max))
    (goto-char (line-beginning-position))
    (search-forward-regexp haskell-interactive-prompt)
    (delete-region (point) (line-end-position))
    (insert p)))

(defun haskell-interactive-buffer ()
  "Get the interactive buffer of the session."
  (haskell-session-interactive-buffer (haskell-session)))

(defun haskell-interactive-show-load-message (session type module-name file-name echo)
  "Show the '(Compiling|Loading) X' message."
  (let* ((file-name-module
          (replace-regexp-in-string
           "\\.hs$" ""
           (replace-regexp-in-string "[\\/]" "." file-name)))
         (msg (ecase type
                ('compiling (format "Compiling: %s" module-name))
                ('loading (format "Loading: %s" module-name)))))
    (haskell-mode-message-line msg)
    (when echo
      (haskell-interactive-mode-echo session msg))))

(defun haskell-interactive-mode-tab ()
  "The tab command."
  (interactive)
  (cond
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

(defun haskell-interactive-mode-error-backward ()
  "Go backward to the previous error."
  (interactive)
  (search-backward-regexp "^[^:]+:[0-9]+:[0-9]+: " nil t))

(defun haskell-interactive-mode-error-forward ()
  "Go forward to the next error, or return to the REPL."
  (interactive)
  (goto-char (line-end-position))
  (if (search-forward-regexp "^[^:]+:[0-9]+:[0-9]+: " nil t)
      (progn (goto-char (line-beginning-position))
             t)
    (progn (goto-char (point-max))
           nil)))

(defun haskell-interactive-mode-visit-error ()
  "Visit the buffer of the current (or last) error message."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (if (progn (goto-char (line-beginning-position))
               (looking-at "^[^:]+:[0-9]+:[0-9]+: "))
        (progn (forward-line -1)
               (haskell-interactive-jump-to-error-line))
      (progn (goto-char (point-max))
             (haskell-interactive-mode-error-backward)
             (haskell-interactive-jump-to-error-line)))))

(defun haskell-interactive-mode-reset-error (session)
  "Reset the error cursor position."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer session)
    (goto-char (point-max))))

(provide 'haskell-interactive-mode)

;;; haskell-interactive-mode.el ends here
