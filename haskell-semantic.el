;;; haskell-semantic.el --- Semantic support for Haskell   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2016 Gracjan Polak

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

;;; Code:

(require 'semantic)
(require 'semantic/ctxt)
(require 'haskell-wy)
(require 'haskell-lexeme)

(defun haskell-semantic-token-lexer (start end &optional _depth length)
  "Semantic compatible lexer.

Returns a list of items where each item describes a token. Each
item has the form:

   (token start . end)

Where token kind is a symbol and start and end are buffer
positions spanning the token.

For more details see `semantic-lex'."

  (save-excursion
    (goto-char start)
    (let ((count 0)
          result)
      (while (and (< (point) end)
                  (or (not length)
                      (< count length)))
        (let* ((tok (haskell-lexeme-looking-at-token))
               (tok2 (cl-case tok
                       (qsymid
                        (cl-case (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))
                          (varsym
                           (cond
                            ((equal (match-string 0) "=") 'EQUAL)
                            ((equal (match-string 0) "@") 'AT)
                            (t 'QVARSYM)))
                          (consym
                           'QCONSYM)
                          (varid
                           'QVARID)
                          (conid
                           'QCONID)))
                       (special
                        (cl-case (char-after (match-beginning 0))
                          (?\( 'OPEN-PAREN)
                          (?\) 'CLOSE-PAREN)
                          (?\[ 'OPEN-BRACKET)
                          (?\] 'CLOSE-BRACKET)
                          (?\{ 'OPEN-BRACE)
                          (?\} 'CLOSE-BRACE)
                          (?\, 'COMMA)
                          (?\; 'SEMICOLON)
                          (?\` 'BACKQUOTE)))
                       (char 'LITERAL)
                       (string 'LITERAL)
                       (number 'LITERAL)
                       (t tok))))
          (when tok2
            (setq count (1+ count))
            (goto-char (match-end 0))
            (when (< (match-beginning 0) end)
              (push (cons tok2 (cons (match-beginning 0) (match-end 0))) result)))))
      (nreverse result))))

(defun haskell-semantic-token-lexer-1 ()
  "Semantic compatible lexer.

Returns a list of items where each item describes a token. Each
item has the form:

   (token start . end)

Where token kind is a symbol and start and end are buffer
positions spanning the token.

For more details see `semantic-lex'."

  (interactive)
  (let* ((tok (haskell-lexeme-looking-at-token))
         (tok2 (cl-case tok
                 (qsymid
                  (cl-case (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))
                    (varsym
                     (cond
                      ((equal (match-string 0) "=") 'EQUAL)
                      (t 'QVARSYM)))
                    (consym
                     'QCONSYM)
                    (varid
                     'QVARID)
                    (conid
                     'QCONID)))
                 (special
                  (cl-case (char-after (match-beginning 0))
                    (?\( 'OPEN-PAREN)
                    (?\) 'CLOSE-PAREN)
                    (?\[ 'OPEN-BRACKET)
                    (?\] 'CLOSE-BRACKET)
                    (?\{ 'OPEN-BRACE)
                    (?\} 'CLOSE-BRACE)
                    (?\, 'COMMA)
                    (?\; 'SEMICOLON)
                    (?\` 'BACKQUOTE)))
                 (char 'LITERAL)
                 (string 'LITERAL)
                 (number 'LITERAL)
                 (t tok))))
    (when tok2
      (goto-char (match-end 0))
      (cons tok2 (cons (match-beginning 0) (match-end 0))))))


(defun haskell-semantic-block-lexer (start end &optional depth length)
  "Semantic compatible lexer.

Returns a list of items where each item describes a token. Each
item has the form:

   (token start . end)

Where token kind is a symbol and start and end are buffer
positions spanning the token.

For more details see `semantic-lex'."

  (save-excursion
    (goto-char start)
    (let ((count 0)
          result)
      (while (and (< (point) end)
                  (or (not length)
                      (< count length))
                  (re-search-forward "^[^ \t\n].*\\(\\(\n[ \t].*\\|\n\\)*\n[ \t].*\\)?\n?" nil t))
        (setq count (1+ count))
        (push (cons 'TOPBLOCK (cons (match-beginning 0) (match-end 0))) result))
      (nreverse result))))

(defun haskell-semantic-lexer (start end &optional depth length)
  "Semantic compatible lexer.

Returns a list of items where each item describes a token. Each
item has the form:

   (token start . end)

Where token kind is a symbol and start and end are buffer
positions spanning the token.

For more details see `semantic-lex'.

Currently it calls `haskell-semantic-token-lexer', in future it
may redirect in reparse circumstances to
`haskell-semantic-block-lexer'. Up till now it has prove too
difficult to get reparsing right."

  (haskell-semantic-token-lexer start end depth length))

(defun haskell-wisent-test ()
  "Use to parse whole buffer and return the parse tree.

Useful for testing purposes. Resets all needed buffer state for
Semantic."
  (interactive)
  (haskell-default-setup)
  (let* ((wisent-parse-verbose-flag t)
         (wisent-lex-istream
          (funcall semantic-lex-analyzer (point-min) (point-max)))
         (wisent-result (wisent-parse semantic--parse-table #'wisent-lex
                                      (lambda (msg) (message "%s" msg)) 'zonk)))
    (message "%S" wisent-result)))

(define-mode-local-override semantic-tag-components
  haskell-mode (tag)
  "Return the children of tag TAG."
  (semantic-tag-get-attribute tag :children))

(defun haskell-semantic-edits-new-change (overlay)
  ;;(message "haskell-semantic-edits-new-change %S" overlay)
  )

(defun haskell-semantic-edits-delete-change (overlay)
  ;;(message "haskell-semantic-edits-delete-change %S" overlay)
  )

(defun haskell-semantic-edits-reparse-change (overlay)
  ;;(message "haskell-semantic-edits-reparse-change %S" overlay)
  )

(defun haskell-default-setup ()
  "Hook run to setup Semantic in `haskell-mode'."
  (interactive)
  (haskell-wy--install-parser)
  (setq
   ;; Lexical analysis
   semantic-lex-number-expression nil ;; semantic-java-number-regexp
   semantic-lex-analyzer 'haskell-semantic-lexer ;; 'wisent-java-tags-lexer
   ;; Parsing
   semantic-tag-expand-function nil ;; #'haskell-semantic-expand-tag
   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character nil ;; '(".")
   semantic-command-separation-character nil ;; ";"
   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts
   ;; in type parts
   '((type     . "Classes")
     (variable . "Variables")
     (function . "Functions"))
   semantic-symbol->name-assoc-list
   ;; everywhere
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((include  . "Imports")
             (package  . "Package")))
   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)
   ;; Remove 'recursive from the default semanticdb find throttle
   ;; since Haskell imports never recurse.
   semanticdb-find-default-throttle
   (remq 'recursive (default-value 'semanticdb-find-default-throttle))
   )
  (semantic-make-local-hook 'semantic-edits-new-change-functions)
  (add-hook 'semantic-edits-new-change-functions
            'haskell-semantic-edits-new-change
            nil t)
  (semantic-make-local-hook 'semantic-edits-delete-change-functions)
  (add-hook 'semantic-edits-delete-change-functions
            'haskell-semantic-edits-delete-change
            nil t)
  (semantic-make-local-hook 'semantic-edits-reparse-change-functions)
  (add-hook 'semantic-edits-reparse-change-functions
            'haskell-semantic-edits-reparse-change
            nil t)
  (global-semantic-decoration-mode))

(push '(haskell-mode . haskell-default-setup) semantic-new-buffer-setup-functions)

(provide 'haskell-semantic)
