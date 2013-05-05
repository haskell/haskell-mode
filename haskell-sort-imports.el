;; haskell-sort-imports.el — Sort the list of Haskell imports at the point alphabetically.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; If the region is active it sorts the imports within the
;; region.

;; This will align and sort the columns of the current import
;; list. It's more or less the coolest thing on the planet.

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

(require 'general-sort)

(defvar haskell-sort-imports-regexp
  (concat "^"
          ;; Optional multi-line comment that precedes the import
          ;; statement.  This must be separated from the import by a
          ;; single newline, or else it doesn't belong to the import.
          "\\([ ]*--.*\n\\)*"
          "import[ ]+"
          "\\(qualified \\)?"
          "[ ]*\\(\"[^\"]*\" \\)?"
          ;; This (4) is the group we sort on.
          "[ ]*\\([A-Za-z0-9_.']*.*\\)"
          ;; This last optional regexp covers the (..) last part of
          ;; the import statement which we allow to cover multiple
          ;; lines.  We don't include it in the regexp field we sort
          ;; on though.
          ;; Note that by default emacs will not return the longest
          ;; match, only the first match, so we need to use the posix
          ;; regexp functions for this part of the regexp to match.
          "\\((\\([^)]\n?\\)*).*\\)?"))

(defvar haskell-sort-imports-stop-regexp
  (concat "^[ ]*\\(::"
          "\\|[a-zA-Z0-9 ]*="
          "\\|[a-zA-Z0-9 ]*<-"
          "\\|[a-zA-Z0-9 ()]*where"
          "\\|type"
          ;; We don't sort ifdefs correctly
          "\\|#[ ]*if"
          "\\|#[ ]*endif"
          ;; Some projects use this comment-style to separate import
          ;; sections.  We shouldn't sort across this boundary.
          "\\|---------------"
          "\\|data\\)")
  "A regexp indicating the beginning/end of a region containing
  import statements.")

;;;###autoload
(defun haskell-sort-imports (&optional silent)
  "Sort the import list at the point."
  (interactive)
  (if (use-region-p)
      (haskell-sort-imports-sort-imports-at (region-beginning)
                                            (region-end))
      (let* ((stop-rx haskell-sort-imports-stop-regexp)
             (import-rx haskell-sort-imports-regexp)
             ;; From current point, where does import region
             ;; begin/end?
             (begin (or (save-excursion
                          (re-search-backward stop-rx (point-min) t))
                        (point-min)))
             (end (or (save-excursion
                        (re-search-forward stop-rx (point-max) t))
                      (point-max)))
             ;; Are there any imports between begin/end - i.e. is
             ;; point in the import region at all?
             (importp (save-excursion
                        (goto-char begin)
                        (posix-search-forward import-rx end t)))
             ;; From a global view of the buffer, where does the
             ;; import region begin/end?
             (global-begin (save-excursion
                             (goto-char (point-min))
                             (posix-search-forward import-rx (point-max) t)))
             (global-end (save-excursion
                           (goto-char (point-max))
                           (posix-search-backward import-rx (point-min) t)))
             ;; Is there any "junk" in the global import area or is it
             ;; clean?
             (global-junkp (save-excursion
                             (goto-char global-begin)
                             (re-search-forward stop-rx global-end t))))

        (cond
          ;; We're in an import section, sort what we see around us.
          (importp
           (let ((current-line (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                 (col (current-column)))
             (haskell-sort-imports-sort-imports-at begin end)
             ;; Try to find back to where we were in the import
             ;; section..
             (let ((line (save-excursion (goto-char end)
                                         (search-backward current-line))))
               (goto-char (+ line col)))))
          ((not (and begin end))
           (or silent (message "No import section found")))
          (global-junkp
           (or silent (message "Junk found in import section, not sorting")))
          (t
           ;; Just sort the global import section
           (haskell-sort-imports-sort-imports-at global-begin global-end))))))


(defun haskell-sort-imports-sort-imports-at (begin end)
  (save-excursion
    (let (sort-fold-case)
      (general-sort-regexp-fields nil
                                  haskell-sort-imports-regexp
                                  "\\4"
                                  begin end
                                  'posix-search-forward))))

(provide 'haskell-sort-imports)
