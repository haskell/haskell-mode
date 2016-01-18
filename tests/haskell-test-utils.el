;;; haskell-test-utils.el --- Utilities for Haskell Mode tests.

;; Copyright © 2016 Arthur Fayzrakhmanov. All rights reserved.

;; This file is part of haskell-mode package.
;; You can contact with authors using GitHub issue tracker:
;; https://github.com/haskell/haskell-mode/issues

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

;; This package provides utilities for `haskell-mode' tests.

;;; Code:

(defun insert-lines (&rest lines)
  "Insert all LINES in current buffer."
  (let ((ls lines)
        (line ""))
    (while ls
      (setq line (car ls))
      (setq ls (cdr ls))
      (when ls
        (insert line)
        (insert "\n")))
    (insert line)))

(defmacro with-temp-switch-to-buffer (&rest body)
  "Create a temporary buffer and evalute BODY there.
Uses `switch-to-buffer' and evaluates BODY in temp buffer like `progn'.

Seems that `execute-kbd-macro' is not able to correctly execute
keybindings without this."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*")))
       ;; FIXME: kill-buffer can change current-buffer in some odd cases.
       (unwind-protect
           (progn
             (switch-to-buffer ,temp-buffer)
             ,@body)
         (and (buffer-name ,temp-buffer)
              (kill-buffer ,temp-buffer))))))

(provide 'haskell-test-utils)
;;; haskell-test-utils.el ends here
