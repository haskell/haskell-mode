;;; haskell-utils.el --- General utility functions used by haskell-mode modules

;; Copyright (C) 2013  Herbert Valerio Riedel

;; Author: Herbert Valerio Riedel <hvr@gnu.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module's purpose is to provide a place for helper functions
;; which are general enough to be usable by multiple modules and/or
;; to alleviate circular module dependency problems.
;;
;; When possible, functions in this module shall be accompanied by
;; ERT-based unit tests.
;;
;; See also `haskell-str.el' for string utility functions.
;;
;; All symbols in this module have a `haskell-utils-' prefix.

;;; Code:

;; NOTE: This module is supposed to be a leaf-module and shall not
;;       require/depend-on any other haskell-mode modules in order to
;;       stay at the bottom of the module dependency graph.

(require 'haskell-customize)

(defvar haskell-utils-async-post-command-flag nil
  "Non-nil means some commands were triggered during async function execution.")
(make-variable-buffer-local 'haskell-utils-async-post-command-flag)

(defun haskell-utils-read-directory-name (prompt default)
  "Read directory name and normalize to true absolute path.
Refer to `read-directory-name' for the meaning of PROMPT and
DEFAULT. If `haskell-process-load-or-reload-prompt' is nil, accept `default'."
  (let ((filename (file-truename
		   (if haskell-process-load-or-reload-prompt
		       (read-directory-name prompt
					    default
					    default)
		     default))))
    (concat (replace-regexp-in-string "/$" "" filename) "/")))

(defun haskell-utils-parse-import-statement-at-point ()
  "Return imported module name if on import statement or nil otherwise.
This currently assumes that the \"import\" keyword and the module
name are on the same line.

This function supports the SafeHaskell and PackageImports syntax extensions.

Note: doesn't detect if in {--}-style comment."
  (save-excursion
    (goto-char (line-beginning-position))
    (if (looking-at (concat "[\t ]*import[\t ]+"
                            "\\(?:safe[\t ]+\\)?" ;; SafeHaskell
                            "\\(?:qualified[\t ]+\\)?"
                            "\\(?:\"[^\"]*\"[\t ]+\\)?" ;; PackageImports
                            "\\([[:digit:][:upper:][:lower:]_.]+\\)"))
        (match-string-no-properties 1))))

(defun haskell-utils-async-update-post-command-flag ()
  "A special hook which collects triggered commands during async execution.
This hook pushes value of variable `this-command' to flag variable
`haskell-utils-async-post-command-flag'."
  (let* ((cmd this-command)
         (updated-flag (cons cmd haskell-utils-async-post-command-flag)))
    (setq haskell-utils-async-post-command-flag updated-flag)))

(defun haskell-utils-async-watch-changes ()
  "Watch for triggered commands during async operation execution.
Resets flag variable
`haskell-utils-async-update-post-command-flag' to NIL.  By chanhges it is
assumed that nothing happened, e.g. nothing was inserted in
buffer, point was not moved, etc.  To collect data `post-command-hook' is used."
  (setq haskell-utils-async-post-command-flag nil)
  (add-hook
   'post-command-hook #'haskell-utils-async-update-post-command-flag nil t))

(defun haskell-utils-async-stop-watching-changes (buffer)
  "Clean up after async operation finished.
This function takes care about cleaning up things made by
`haskell-utils-async-watch-changes'.  The BUFFER argument is a buffer where
`post-command-hook' should be disabled.  This is neccessary, because
it is possible that user will change buffer during async function
execusion."
  (with-current-buffer buffer
    (setq haskell-utils-async-post-command-flag nil)
    (remove-hook
     'post-command-hook #'haskell-utils-async-update-post-command-flag t)))

(defun haskell-utils-reduce-string (s)
  "Remove newlines ans extra whitespace from S.
Removes all extra whitespace at the beginning of each line leaving
only single one.  Then removes all newlines."
  (let ((s_ (replace-regexp-in-string "^\s+" " " s)))
    (replace-regexp-in-string "\n" "" s_)))

(defun haskell-utils-parse-repl-response (r)
  "Parse response R from REPL and return special kind of result.
The result is response string itself with speacial property
response-type added.

This property could be of the following:

+ unknown-command
+ option-missing
+ interactive-error
+ success"
  (let ((first-line (car (split-string r "\n"))))
    (cond
     ((string-match-p "^unknown command" first-line) 'unknown-command)
     ((string-match-p "^Couldn't guess that module name. Does it exist?"
                      first-line)
      'option-missing)
     ((string-match-p "^<interactive>:" first-line) 'interactive-error)
     (t 'success))))

(defun haskell-utils-compose-type-at-command (pos)
  "Prepare :type-at command to be send to haskell process.
POS is a cons cell containing min and max positions, i.e. target
expression bounds."
  (save-excursion
    (let ((start-p (car pos))
          (end-p (cdr pos))
          start-l
          start-c
          end-l
          end-c
          value)
      (goto-char start-p)
      (setq start-l (line-number-at-pos))
      (setq start-c (1+ (current-column)))
      (goto-char end-p)
      (setq end-l (line-number-at-pos))
      (setq end-c (1+ (current-column)))
      (setq value (buffer-substring-no-properties start-p end-p))
      ;; supress multiline expressions
      (let ((lines (split-string value "\n" t)))
        (when (and (cdr lines)
                   (stringp (car lines)))
          (setq value (format "[ %s … ]" (car lines)))))
      (replace-regexp-in-string
       "\n$"
       ""
       (format ":type-at %s %d %d %d %d %s"
               (buffer-file-name)
               start-l
               start-c
               end-l
               end-c
               value)))))

(provide 'haskell-utils)
;;; haskell-utils.el ends here
