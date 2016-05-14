;;  -*- lexical-binding: t -*-

;; require 'el-search only on Emacs 25 or above
(when (>= emacs-major-version 25)
  (package-initialize)
  (unless (package-installed-p 'el-search)
    (package-refresh-contents)
    (package-install 'el-search))
  (require 'el-search))

(defun haskell-check-conventions-in-current-buffer ()
  "Check elisp convention suitable for haskell-mode in current buffer."
  (interactive)

  (let (fail-flag)
    (save-excursion
      ;; check lexical binding
      (goto-char (point-min))
      (unless (re-search-forward "lexical-binding: t" (line-end-position) t)
        (unless (member (file-name-nondirectory (buffer-file-name))
                        '("haskell-wy.el" "haskell-mode-pkg.el" "haskell-mode-autoloads.el"))
          (message "%s:1:0: Error: Use `lexical-binding: t` on the first line of file"
                   (buffer-file-name))
          (setq fail-flag t)))
      ;; check TABS
      (goto-char (point-min))
      (while (re-search-forward "\t" nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (message "%s:%d:%d: Error: Do not use literal TAB characters"
                   (buffer-file-name) (line-number-at-pos)
                   (current-column))
          (setq fail-flag t)))
      ;; check spaces or tabs at the end of line
      (goto-char (point-min))
      (while (re-search-forward "[\t ]+$" nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (message "%s:%d:%d: Error: Do not use TABs or spaces at the end of line"
                   (buffer-file-name) (line-number-at-pos)
                   (current-column)))
          (setq fail-flag t))
      ;; check constructs we do not like
      (when (featurep 'el-search)

        (goto-char (point-min))
        (while (el-search--search-pattern `'(require 'cl) t)
          (unless (save-excursion
                    (goto-char (line-beginning-position))
                    (re-search-forward "el-search--search-pattern" (line-end-position) t))
            (message "%s:%d:%d: Error: Do not use (require 'cl) use (require 'cl-lib) instead"
                     (buffer-file-name) (line-number-at-pos)
                     (current-column))
            (setq fail-flag t))
          (el-search--skip-expression nil t))

        ;; check against shell-command
        (goto-char (point-min))
        (while (el-search--search-pattern '(or `(shell-command . ,_) `(shell-command-to-string . ,_)) t)
          (unless (save-excursion
                    (goto-char (line-beginning-position))
                    (re-search-forward "el-search--search-pattern" (line-end-position) t))
            ;; for now this is a warning
            (message "%s:%d:%d: Warning: Do not use (shell-command[-to-string] ...) use (call-process ...) or (start-process ...) instead"
                     (buffer-file-name) (line-number-at-pos)
                     (current-column)))
          (el-search--skip-expression nil t))

        ;; check against message with non-constant first argument
        (goto-char (point-min))
        (while (el-search--search-pattern '(and `(message ,msg . ,_) (guard (not (stringp `,msg)))) t)
          (unless (save-excursion
                    (goto-char (line-beginning-position))
                    (re-search-forward "el-search--search-pattern" (line-end-position) t))
            ;; for now this is a warning
            (message "%s:%d:%d: Warning: First argument to (message ...) must be constant string"
                     (buffer-file-name) (line-number-at-pos)
                     (current-column)))
          (el-search--skip-expression nil t))

        ;; check against non-toplevel add-to-list that has issues working with lexical variables
        (goto-char (point-min))
        (while (el-search--search-pattern '(and `(add-to-list (quote ,var) . ,_)
                                                (guard (not (member `,var '(completion-ignored-extensions
                                                                            auto-mode-alist
                                                                            interpreter-mode-alist
                                                                            minor-mode-overriding-map-alist
                                                                            haskell-sessions
                                                                            flymake-allowed-file-name-masks
                                                                            haskell-cabal-buffers))))) t)
          (unless (save-excursion
                    (goto-char (line-beginning-position))
                    (re-search-forward "el-search--search-pattern" (line-end-position) t))
            (message "%s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
            (message "%s:%d:%d: Error: add-to-list is only allowed on whitelisted variables because it does not work with lexical-binding, use cl-pushnew with :test #'equal"
                     (buffer-file-name) (line-number-at-pos)
                     (current-column))
            (setq fail-flag t))
          (el-search--skip-expression nil t))))
    fail-flag))

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
      (with-current-buffer
          (find-file-noselect file)
        (when (haskell-check-conventions-in-current-buffer)
          (setq fail-flag t))
  (kill-buffer (current-buffer))))
    fail-flag))

(defun haskell-check-conventions-batch-and-exit ()
  "Check elisp coding conventions in elisp files comprising haskell-mode.

Return non-zero exit code when conventions were violated in any of the files."

  (interactive)
  (if (haskell-check-conventions)
      (kill-emacs 2)
    (kill-emacs 0)))
