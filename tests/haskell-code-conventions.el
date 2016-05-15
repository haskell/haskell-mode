;;  -*- lexical-binding: t -*-

;; require 'el-search only on Emacs 25 or above
(when (>= emacs-major-version 25)
  (package-initialize)
  (unless (package-installed-p 'el-search)
    (package-refresh-contents)
    (package-install 'el-search))
  (require 'el-search))

(defconst haskell-code-conventions
  '((require-cl
     error `(require 'cl)
     "Do not use (require 'cl) use (require 'cl-lib) instead")
    (shell-command
     ;; requires thought put into this
     disabled (or `(shell-command . ,_) `(shell-command-to-string . ,_))
     "Do not use (shell-command[-to-string] ...) use (call-process ...) or (start-process ...) instead")
    (message-non-const
     error (and `(message ,msg . ,_) (guard (not (stringp `,msg))))
     "First argument to (message ...) should be a string literal")
    (add-to-list
     error (and `(add-to-list (quote ,var) . ,_)
                  (guard (not (member `,var '(completion-ignored-extensions
                                              auto-mode-alist
                                              interpreter-mode-alist
                                              minor-mode-overriding-map-alist
                                              haskell-sessions
                                              flymake-allowed-file-name-masks
                                              haskell-cabal-buffers)))))
     "Use add-to-list only with dynamically scoped variables, whitelist is in `haskell-code-conventions.el'")
    (when-not ;; noisy
     disabled `(when (not . ,_) . ,_)
     "Use (unless ...) instead of (when (not ...))")
    (unless-not ;; noisy
     disabled `(unless (not . ,_) . ,_)
     "Use (when ...) instead of (unless (not ...))")
    (nested-when-or-unless ;; noisy
     disabled `(,(or `when `unless) ,_ (,(or `when `unless) . ,_))
     "Nested when/unless, consider using logical operators instead of nesting")
    (if-progn-one-branch
     disabled `(if ,_ (progn . ,_))
     "Use `when` for `if+progn` with one branch only")
    (add-hook-function
     disabled `(add-hook ,_ (function ,_) . ,_)
     "Use `add-hook` without #-notation, just symbols because those should be autoloaded")))

(defun haskell-check-char-conventions-in-current-buffer ()
  "Check elisp convention suitable for haskell-mode in current buffer."
  (interactive)

  (let (fail-flag)
    (save-excursion
      ;; check lexical binding
      (goto-char (point-min))
      (unless (re-search-forward "lexical-binding: t" (line-end-position) t)

        (message "%s:1:0: Error: Use `lexical-binding: t` on the first line of file"
                 (file-relative-name (buffer-file-name)
                                     (with-current-buffer "*scratch*"
                                       default-directory)))
        (setq fail-flag t))

      ;; check TABS
      (goto-char (point-min))
      (while (re-search-forward "\t" nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (message "%s:%d:%d: Error: Do not use literal TAB characters"
                   (file-relative-name (buffer-file-name)
                                       (with-current-buffer "*scratch*"
                                         default-directory)) (line-number-at-pos)
                   (current-column))
          (setq fail-flag t)))
      ;; check spaces or tabs at the end of line
      (goto-char (point-min))
      (while (re-search-forward "[\t ]+$" nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (message "%s:%d:%d: Error: Do not use TABs or spaces at the end of line"
                   (file-relative-name (buffer-file-name)
                                       (with-current-buffer "*scratch*"
                                         default-directory)) (line-number-at-pos)
                   (current-column)))
        (setq fail-flag t)))

    fail-flag))

(defun haskell-check-sexp-conventions-in-current-buffer ()
  "Check elisp convention suitable for haskell-mode in current buffer."
  (interactive)

  (when (featurep 'el-search)
    (let (fail-flag)
      (save-excursion
        (dolist (convention haskell-code-conventions)
          (unless (equal 'disabled (nth 1 convention))
            (goto-char (point-min))
            (while (el-search--search-pattern `(and ,(nth 2 convention) (guard (not (save-excursion
                                                                                      (beginning-of-defun)
                                                                                      (looking-at-p "(defconst haskell-code-conventions"))))) t)
              (message "%s:%d:%d: %s: %s"
                       (file-relative-name (buffer-file-name)
                                           (with-current-buffer "*scratch*"
                                             default-directory)) (line-number-at-pos)
                                             (current-column)
                                             (if (equal 'error (nth 1 convention)) "Error" "Warning")
                                             (nth 3 convention))
              (message "%s"
                       (replace-regexp-in-string
                        "^" "  " (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (save-excursion
                                    (goto-char (el-search--end-of-sexp))
                                    (line-end-position)))))
              (when (equal 'error (nth 1 convention))
                (setq fail-flag t))
              (el-search--skip-expression nil t)))))

      fail-flag)))

(defun haskell-check-conventions-in-current-buffer ()
  "Check elisp convention suitable for haskell-mode in current buffer."
  (interactive)
  (or (haskell-check-char-conventions-in-current-buffer)
      (haskell-check-sexp-conventions-in-current-buffer)))

(defun haskell-check-conventions ()
  "Check elisp coding conventions in elisp files comprising haskell-mode.

Return non-nil when conventions were violated in any of the files."

  (interactive)
  (let* ((this-script-directory
    (file-name-directory
     (or (symbol-file 'haskell-check-conventions)
         (buffer-file-name))))
   (files
    (append (directory-files (concat this-script-directory "/..") t ".*\\.el$" t)
      (directory-files this-script-directory t ".*\\.el$" t)))
         fail-flag)

    (dolist (file files)
      (unless (member (file-name-nondirectory file)
                      '("haskell-wy.el" "haskell-mode-pkg.el" "haskell-mode-autoloads.el"
                        ;; old files we do not care about
                        "haskell-indent.el" "haskell-decl-scan.el"))
        (with-current-buffer
            (find-file-noselect file)
          (when (haskell-check-conventions-in-current-buffer)
            (setq fail-flag t))
          (kill-buffer (current-buffer)))))
    fail-flag))

(defun haskell-check-conventions-batch-and-exit ()
  "Check elisp coding conventions in elisp files comprising haskell-mode.

Return non-zero exit code when conventions were violated in any of the files."

  (interactive)
  (if (haskell-check-conventions)
      (kill-emacs 2)
    (kill-emacs 0)))
