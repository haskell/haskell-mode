;;; haskell-navigate-imports.el --- A function for cycling through Haskell import lists -*- lexical-binding: t -*-

;; Copyright (C) 2016  Bradley Hardy

;; Author: Bradley Hardy <bradleyhardy@live.com>

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; Use of the exported interactive functions should never leave the imports in
;; an invalid state, as long as all names provided by the user exist in the
;; right place. Adding an unqualified import twice will merge the sub-imports.

;; This module works independently of most of the rest of haskell-mode, just
;; requiring haskell-navigate-imports.

;; Exports four interactive functions:
;; 1. haskell-auto-import-unqualified
;; 2. haskell-auto-import-unqualified-all
;; 3. haskell-auto-import-qualified
;; 4. haskell-auto-import-qualified-and-unqualified

;; Example usage:

;; (require 'haskell-auto-import)
;; (define-key haskell-mode-map (kbd "C-c i i") 'haskell-auto-import-unqualified)
;; (define-key haskell-mode-map (kbd "C-c i a") 'haskell-auto-import-unqualified-all)
;; (define-key haskell-mode-map (kbd "C-c i q") 'haskell-auto-import-qualified)
;; (define-key haskell-mode-map (kbd "C-c i x") 'haskell-auto-import-qualified-and-unqualified)

;;; Code:

(require 'haskell-navigate-imports)
(require 'cl-lib)

(defun haskell-auto-import-unqualified (module-name sub-imports)
  "Add a new unqualified import of the provided module and sub-imports. If
sub-imports is left blank, import everything in the module."
  (interactive "MModule Name: \nMSub-Imports (comma separated, leave blank to import all): ")
  (let* ((all-infos (haskell-auto-import-module-import-info module-name))
         (non-qualified (member-if '(lambda (info) (not (car (cdr (cdr info))))) all-infos))
         (new-sub-imports-list
          (haskell-auto-import-parse-import-parens sub-imports)))
    (if non-qualified
        (let* ((info (car non-qualified))
               (keep-info (butlast info 1))
               (imported-items (car (last info 1)))
               (new-info
                (append keep-info
                        (if new-sub-imports-list
                            (list
                             (haskell-auto-import-normalize-imported-items
                              (append imported-items new-sub-imports-list)))
                          ()
                            ))))
          (haskell-auto-import-replace-import module-name new-info))
      (progn
        (let ((old-point (point)))
          (if all-infos
              (goto-char (+ 1 (car (car all-infos))))
            (haskell-navigate-imports))

          (insert
           (haskell-auto-import-print-module-import
            module-name
            (list nil nil new-sub-imports-list)))
          (newline)

          (goto-char old-point))))))

(defun haskell-auto-import-unqualified-all (module-name)
  "Add a new unqualified import of the provided module. Everything in that
module is imported."
  (interactive "MModule Name: ")
  (haskell-auto-import-unqualified module-name ""))

(defun haskell-auto-import-qualified (module-name qualifier)
  "Add a new qualified import of the provided module with the provided
qualifier. Attempts to place the new import next to existing imports of the same
module if they exist."
  (interactive "MModule Name: \nMQualifier: ")
  (let* ((all-infos (haskell-auto-import-module-import-info module-name))
         (qualified
          (member-if '(lambda (info)
                        (car (cdr (cdr info))))
                     all-infos))
         (same-qualifier
          (member-if '(lambda (info)
                        (string= (car (cdr (cdr info))) qualifier))
                     all-infos)))
    (if same-qualifier
        (message "That module is already imported with the same qualifier. Doing nothing.")
      (let ((old-point (point)))
        ;; If the module is already imported in some capacity, put the new
        ;; import above the existing ones
        (if all-infos
            (goto-char (+ 1 (car (car all-infos))))
          (haskell-navigate-imports))

        (insert
         (haskell-auto-import-print-module-import
          module-name
          (list t qualifier nil)))
        (newline)

        (goto-char old-point)))))

(defun haskell-auto-import-qualified-and-unqualified (module-name qualifier sub-imports)
  "Add a new import, both qualified and unqualified. For example, given the
module `Data.Map', qualifier `Map' and sub-imports `Map', this will add the lines:

import Data.Map (Map)
import qualified Data.Map as Map"

  (interactive
   "MModule Name: \nMQualifier: \nMSub-Imports (comma separated, leave blank to import all): ")
  (haskell-auto-import-qualified module-name qualifier)
  (haskell-auto-import-unqualified module-name sub-imports))

(defun haskell-auto-import-parse-import-parens (contents)
  "Given the contents of some import parentheses, return a list of pairs which
each consist of '(imported-item sub-imports), where sub-imports is the list of
items imported from the main item (e.g. in `import ModuleName(DataType(Con1,
Con2))')

Does NOT currently work properly with datatype operators with sub-imports (e.g.
`import Foo.Bar ((**)(A, B))') or with sub-imports which are operators (e.g.
`import Foo.Bar (A((&&), (||)))') "
  (let* ((exploded (split-string contents "," nil "\\s-"))
         (parts
          (reduce
           '(lambda (acc str)
              (let ((cur (car acc))
                    (rest (cdr acc))
                    (strlist (string-to-list str)))
                (if cur
                    (if (member ?\) strlist)
                        (cons () (cons (cons str cur) rest))
                      (cons (cons str cur) rest))
                  (if (and
                       (member ?\( strlist)
                       ;; If the close paren is present too, we have a whole
                       ;; token
                       (not (member ?\) strlist)))
                      (cons (list str) rest)
                    (cons () (cons (list str) rest))))))
           exploded
           :initial-value (list ()))))
    (mapcar
     '(lambda (xs)
        (if (equal (string-to-char (car xs)) ?\()
            xs
          (reduce 'append
                  (mapcar
                   '(lambda (s)
                      (split-string s "[()]" t "\\s-"))
                   (reverse xs)))))
     (reverse (cdr parts)))))

(defun haskell-auto-import-module-single-import-info (module-name &optional start)
  "Returns information about the first import of the given module after `start'
(which is set to 0 if nil). This information is nil if the module is not
imported. Otherwise it is a list consisting of `(list start-point end-point
is-qualified qualified-name imported-items)' where:
- `start-point' is the point at the start of the import line
- `end-point' is the point at the end of the import line
- `is-qualified' is `t' if the import is qualified, nil otherwise
- `qualified-name' is nil if no `as' clause was specified, or the qualified name
  otherwise.
- `imported-items' is a list of items imported from the module, in the format
  provided by `haskell-auto-import-parse-import-parens' if it exists, nil otherwise."
  (let ((contents (buffer-string))
        (regexp
         (concat
          ;; Match the import keyword followed by whitespace
          "^import\\s-+"
          ;; Optionally match the qualified keyword as group 1
          "\\(qualified\\s-+\\)?"
          ;; Match exactly the module name
          (regexp-quote module-name)
          ;; Optionally match an `as' qualifier as group 2
          "\\(?:\\s-+as\\s-+\\(\\sw+\\)\\)?"
          ;; Whitespace may or may not precede parentheses at the end
          "\\s-*"
          ;; Optionally match the contents of some following parentheses as
          ;; group 3
          "\\(?:(\\(\\(?:[a-zA-Z0-9', \t\f\n\r]*\\|(.*?)\\)*\\))\\)?"
          )))
    (if (string-match regexp contents start)
        (let ((is-qualified (match-string 1 contents))
              (qualified-name (match-string 2 contents))
              (imported-items-str (match-string 3 contents))
              (start-point (match-beginning 0))
              (end-point (match-end 0)))
          (list start-point end-point
                (if is-qualified t nil)
                qualified-name
                (if imported-items-str
                    (haskell-auto-import-parse-import-parens imported-items-str)
                  nil)
                )))))

(defun haskell-auto-import-module-import-info (module-name &optional start)
  "Returns information about every import of the given module after `start'
(which is set to 0 if nil). Each member of the returned list has the same format
as that returned from `haskell-auto-import-module-single-import-info'."
  (let ((next-info (haskell-auto-import-module-single-import-info module-name start)))
    (if next-info
        (let ((next-start (car (cdr next-info))))
          (cons next-info (haskell-auto-import-module-import-info module-name next-start)))
      ()
      )))

(defun haskell-auto-import-normalize-imported-items (imported-items)
  "Normalize imported items, e.g. `one(a), two(a, b), two(a, c), one' --> `one,
two(a, b, c)'"
  (let ((grouped (-group-by 'car imported-items)))
    (mapcar
     '(lambda (item-group)
        (let* ((group-main (car item-group))
               (all-subs (mapcar 'cdr (cdr item-group)))
               (flat-subs (reduce 'append all-subs))
               (distinct-subs (-distinct flat-subs)))
          (if (member nil all-subs)
              (list group-main)
            (cons group-main distinct-subs)
            )))
     grouped)))

(defun haskell-auto-import-print-imported-items (imported-items)
  (mapconcat
   '(lambda (item)
      (let ((imported-item (car item))
            (sub-imports (cdr item)))
        (concat
         imported-item
         (if sub-imports
             (concat
              "("
              (mapconcat 'identity sub-imports ", ")
              ")")
             ""))
        ))
   imported-items ", "))

(defun haskell-auto-import-print-module-import (module-name info)
  (let ((is-qualified (car info))
        (qualified-name (car (cdr info)))
        (imported-items (car (cdr (cdr info)))))
    (concat
     "import "
     (if is-qualified "qualified " "")
     module-name
     (if qualified-name (concat " as " qualified-name) "")
     (if imported-items
         (concat " (" (haskell-auto-import-print-imported-items imported-items) ")")
       "")
     )))

(defun haskell-auto-import-replace-import (module-name info)
  (let ((start-point (car info))
        (end-point (car (cdr info)))
        (rest-info (cdr (cdr info)))
        (old-point (point))
        )
    (delete-region (+ 1 start-point) (+ 1 end-point))
    (goto-char (+ 1 start-point))
    (insert (haskell-auto-import-print-module-import module-name rest-info))
    (goto-char old-point)))

(provide 'haskell-auto-import)
