;;; haskell-collapse.el --- Collapse expressions -*- lexical-binding: t -*-

;; Copyright (c) 2014 Chris Done. All rights reserved.
;; Copyright (c) 2017 Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>.

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

(require 'hideshow)

(defun haskell-hide-toggle ()
  "Toggle visibility of existing forms at point. "
  (interactive)
  (hs-minor-mode 1)
  (save-excursion
    (let* ((modified (buffer-modified-p))
           (inhibit-read-only t)
	   (position (haskell-indented-block))
	   (beg (car position))
	   (end (cdr position)))
      (if (and beg end)
          (if (overlays-in beg end)
              (hs-discard-overlays beg end)
            (hs-make-overlay beg end 'code)))
      (set-buffer-modified-p modified))))

(defun haskell-blank-line-p (&optional pos)
  "Returns `t' if line (optionally, line at POS) is empty or
composed only of whitespace."
  (save-excursion
    (goto-char (or pos (point)))
    (beginning-of-line)
    (= (point-at-eol)
       (progn (skip-syntax-forward " ") (point)))))

(defun haskell-indented-block ()
  "return (start-of-indentation . end-of-indentation)"
  (let ((cur-indent (current-indentation))
	(nxt-line-indent (haskell-next-line-indentation))
	(prev-line-indent (haskell-prev-line-indentation))
	(beg-of-line (save-excursion (end-of-line)
				     (point))))
    (cond ((= cur-indent nxt-line-indent 0) nil)
	  ((haskell-blank-line-p) nil)
	  ((> nxt-line-indent cur-indent)
	   (cons beg-of-line
		 (haskell-find-line-with-indentation '> 1)))
	  ((or (= nxt-line-indent cur-indent)
	       (<= prev-line-indent cur-indent))
	   (cons (haskell-find-line-with-indentation '>= 1)
		 (haskell-find-line-with-indentation '>= -1)))
	  (t (error "Undefined behaviour")))))

(defun haskell-next-line-indentation ()
  "returns (integer) indentation of the next line"
  (save-excursion
    (forward-line 1)
    (current-indentation)))

(defun haskell-prev-line-indentation ()
    "returns (integer) indentation of the previous line"
  (save-excursion
    (forward-line -1)
    (current-indentation)))

(defun haskell-find-line-with-indentation (comparison direction)
  "comparison is >= or >, direction if 1 finds forward, if -1 finds backward"
  (save-excursion
    (let ((start-indent (current-indentation)))
      (progn
	(while (and (zerop (forward-line direction))
		    (or (haskell-blank-line-p)
			(funcall comparison (current-indentation) start-indent))))
	(when (= direction 1) (forward-line -1))
	(end-of-line)
	(point)))))

(provide 'haskell-collapse)
