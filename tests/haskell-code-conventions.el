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
