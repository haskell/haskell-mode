;;; general-sort.el --- General field sorting function

;; Copyright (C) 2013  Alexander Kjeldaas

;; Author: Alexander Kjeldaas <astor@astor-SATELLITE-C870-147>
;; Keywords: unix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a general fields sorting function much like
;; `sort-regexp-fields', but with an extra argument that selects which
;; regexp function to use.  Emacs provides both fast-and-erroneous and
;; slow-and-correct versions (the default and the posix version).

;;; Code:

(require 'sort)

;;;###autoload
(defun general-sort-regexp-fields (reverse record-regexp key-regexp beg end
                                   rx-fn)
  "Sort the region lexicographically as specified by RECORD-REGEXP and KEY.

RECORD-REGEXP specifies the textual units which should be sorted.
  For example, to sort lines RECORD-REGEXP would be \"^.*$\"
KEY specifies the part of each record (ie each match for RECORD-REGEXP)
  is to be used for sorting.
  If it is \"\\\\digit\" then the digit'th \"\\\\(...\\\\)\" match field from
  RECORD-REGEXP is used.
  If it is \"\\\\&\" then the whole record is used.
  Otherwise, it is a regular-expression for which to search within the record.
If a match for KEY is not found within a record then that record is ignored.

With a negative prefix arg sorts in reverse order.

The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

For example: to sort lines in the region by the first word on each line
 starting with the letter \"f\",
 RECORD-REGEXP would be \"^.*$\" and KEY would be \"\\\\=\\<f\\\\w*\\\\>\"

RX-FN is the forward regexp search function to use.  Typically
this should be `re-search-forward' or `posix-search-forward'"
  ;; using negative prefix arg to mean "reverse" is now inconsistent with
  ;; other sort-.*fields functions but then again this was before, since it
  ;; didn't use the magnitude of the arg to specify anything.
  (interactive "P\nsRegexp specifying records to sort:
sRegexp specifying key within record: \nr")
  (cond ((or (equal key-regexp "") (equal key-regexp "\\&"))
	 (setq key-regexp 0))
	((string-match "\\`\\\\[1-9]\\'" key-regexp)
	 (setq key-regexp (- (aref key-regexp 1) ?0))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (sort-regexp-record-endn
	    (sort-regexp-fields-regexp record-regexp))
	(funcall rx-fn sort-regexp-fields-regexp nil t)
	(setq sort-regexp-record-end (point))
	(goto-char (match-beginning 0))
	(sort-subr reverse
                   (function (lambda ()
                     (general-sort-regexp-fields-next-record rx-fn)))
		   (function (lambda ()
			       (goto-char sort-regexp-record-end)))
		   (function (lambda ()
			       (let ((n 0))
				 (cond ((numberp key-regexp)
					(setq n key-regexp))
				       ((funcall rx-fn
					  key-regexp sort-regexp-record-end t)
					(setq n 0))
				       (t (throw 'key nil)))
				 (condition-case ()
				     (cons (match-beginning n)
					   (match-end n))
				   ;; if there was no such register
				   (error (throw 'key nil)))))))))))

;; Move to the beginning of the next match for record-regexp,
;; and set sort-regexp-record-end to the end of that match.
;; If the next match is empty and does not advance point,
;; skip one character and try again.
(defun general-sort-regexp-fields-next-record (rx-fn)
  (let ((oldpos (point)))
    (and (funcall rx-fn sort-regexp-fields-regexp nil 'move)
	 (setq sort-regexp-record-end (match-end 0))
	 (if (= sort-regexp-record-end oldpos)
	     (progn
	       (forward-char 1)
	       (funcall rx-fn sort-regexp-fields-regexp nil 'move)
	       (setq sort-regexp-record-end (match-end 0)))
	   t)
	 (goto-char (match-beginning 0)))))

(provide 'general-sort)
;;; general-sort.el ends here
