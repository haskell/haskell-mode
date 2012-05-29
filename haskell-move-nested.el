;; haskell-move-nested.el — Change the column of text nested below a line.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; This module is intended for Haskell mode users, but is
;; independent of Haskell mode.

;; Example usage:

;; (require 'haskell-move-nested)
;; (define-key haskell-mode-map (kbd "C-<left>")
;;   (lambda ()
;;     (interactive)
;;     (haskell-move-nested -1)))

;; (define-key haskell-mode-map (kbd "C-<right>")
;;   (lambda ()
;;     (interactive)
;;     (haskell-move-nested 1)))


;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;;###autoload
(defun haskell-move-nested (columns)
  (save-excursion
    (let ((region (haskell-move-nested-region)))
      (when region
        (indent-rigidly (car region) (cdr region) columns)))))

(defun haskell-move-nested-region ()
  (save-excursion
    (let ((starting-level (current-column)))
      (forward-line)
      (let ((current-level (haskell-move-nested-indent-level)))
        (let ((start-point (line-beginning-position))
              (start-end-point (line-end-position))
              (end-point nil)
              (last-line 0))
          (forward-line)
          (while (and (not (= (line-beginning-position) last-line))
                      (or (> (haskell-move-nested-indent-level) starting-level)
                          (and (> current-level starting-level)
                               (>= (haskell-move-nested-indent-level) current-level))))
            (setq last-line (line-beginning-position))
            (setq end-point (line-end-position))
            (forward-line))
          (cons start-point (or end-point
                                start-end-point)))))))

(defun haskell-move-nested-indent-level ()
  (max 
   0 
   (1- (length
        (buffer-substring-no-properties
         (line-beginning-position)
         (or (save-excursion (goto-char (line-beginning-position))
                             (search-forward-regexp "[^ ]" (line-end-position) t 1))
             (line-beginning-position)))))))

(defun haskell-kill-nested ()
  "Kill the nested region after this."
  (interactive)
  (let ((start (point))
        (reg (save-excursion
               (search-backward-regexp "^[ ]+" (line-beginning-position) t 1)
               (search-forward-regexp "[^ ]" (line-end-position) t 1)
               (haskell-move-nested-region))))
    (kill-region start (cdr reg))))

(defun haskell-delete-nested ()
  "Kill the nested region after this."
  (interactive)
  (let ((start (point))
        (reg (save-excursion
               (search-backward-regexp "^[ ]+" (line-beginning-position) t 1)
               (search-forward-regexp "[^ ]" (line-end-position) t 1)
               (haskell-move-nested-region))))
    (delete-region start (cdr reg))))

(provide 'haskell-move-nested)
