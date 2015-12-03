;;; haskell-tab-indent.el --- tab-based indentation for haskell-mode

;; Copyright (C) 2015  Sean Whitton

;; Author: Sean Whitton <spwhitton@spwhitton.name>
;; Keywords: indentation haskell

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;; This file provides `haskell-tab-indent-mode', a simple indentation
;; mode for Haskell projects which require tabs for indentation and do
;; not permit spaces (except for where clauses, as a special case).
;;
;; The user may use TAB to cycle between possible indentations.
;;
;; Installation:
;;
;; If you set `indent-tabs-mode' in the .dir-locals.el file for a
;; project requiring tabs, you can use something like this in your
;; init file to enable this mode for such projects:
;;
;;    (add-hook 'haskell-mode-hook
;;                (lambda ()
;;                  (add-hook 'hack-local-variables-hook
;;                            (lambda ()
;;                              (if indent-tabs-mode
;;                                  (haskell-tab-indent-mode)
;;                                (haskell-indentation-mode)))
;;                            nil t))) ; local hook

;;; Code:

(defun haskell-tab-indent ()
  "Auto indentation on TAB for `haskell-tab-indent-mode'."
  (interactive)
  (save-excursion
    (back-to-indentation)
    ;; check for special case of where clause
    (if (looking-at "where")
	(haskell-tab-indent--where)
      ;; check for special case of being called by
      ;; `newline-and-indent': if the user has `electric-indent-mode'
      ;; on and RET bound to `newline-and-indent', we'll end up
      ;; indenting too far
      (unless (let ((previous-line-tabs (haskell-tab-indent--previous-line-tabs))
                    (this-line-tabs (haskell-tab-indent--this-line-tabs)))
                (and (equal this-command 'newline-and-indent)
                     (= this-line-tabs previous-line-tabs)))
        (haskell-tab-indent--cycle))))
  ;; On a line with only indentation, ensure point is at the end of
  ;; it.
  (when (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$"))
    (end-of-line)))

(defun haskell-tab-indent--where ()
  ;; `haskell-tab-indent' leaves us just after the indentation
  (delete-region (line-beginning-position) (point))
  (insert "  "))

(defun haskell-tab-indent--cycle ()
  (let ((previous-line-tabs (haskell-tab-indent--previous-line-tabs))
        (this-line-tabs (haskell-tab-indent--this-line-tabs)))
    (if (= (1+ previous-line-tabs) this-line-tabs)
        (haskell-tab-indent--reset)
      (haskell-tab-indent--indent))))

(defun haskell-tab-indent--reset ()
  (save-excursion
    (back-to-indentation)
    (delete-region (line-beginning-position) (point))))

(defun haskell-tab-indent--indent ()
  (save-excursion
    (back-to-indentation)
    (insert "\t")))

(defun haskell-tab-indent--previous-line-tabs ()
  (save-excursion
    (beginning-of-line 0)               ; go up one line
    ;; keep going up past blank spacer lines
    (while (looking-at "[[:space:]]*$") (beginning-of-line 0))
    (haskell-tab-indent--this-line-tabs)))

(defun haskell-tab-indent--this-line-tabs ()
  (save-excursion
    (save-restriction
      (back-to-indentation)
      (narrow-to-region (line-beginning-position) (point))
      (beginning-of-line)
      (let ((count 0))
	(while (re-search-forward "\t" nil t)
	  (setq count (1+ count)))
	count))))

;;;###autoload
(define-minor-mode haskell-tab-indent-mode
  "Haskell indentation mode for projects requiring that only tabs
-- with no spaces -- be used for indentation.

Binds the TAB key to cycle between possible indents."
  :lighter " TabInd"
  (kill-local-variable 'indent-line-function)
  (when haskell-tab-indent-mode
    (set (make-local-variable 'indent-line-function) 'haskell-tab-indent)
    (set (make-local-variable 'indent-tabs-mode) t)))

(provide 'haskell-tab-indent)
;;; haskell-tab-indent.el ends here
