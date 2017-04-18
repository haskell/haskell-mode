;;; haskell-wy.el --- Generated parser support file

;; Copyright (C) 2016 Gracjan Polak

;; Author: Gracjan Polak <gracjan@MacBook-Pro-3.local>
;; Created: 2016-03-23 20:11:01+0100
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file haskell.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(defconst haskell-wy--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst haskell-wy--token-table
  (semantic-lex-make-type-table
   '(("string"
      (NEVER)
      (WHERE)
      (UNDERSCORE)
      (DOUBLE-ARROW)
      (TILDE)
      (AT)
      (RIGHT-ARROW)
      (LEFT-ARROW)
      (BACKQUOTE)
      (VERTICAL-BAR)
      (LAMBDA)
      (EQUAL)
      (DOUBLE-COLON)
      (COLON)
      (DOTDOT)
      (SEMICOLON)
      (COMMA)
      (CLOSE-BRACE)
      (OPEN-BRACE)
      (CLOSE-BRACKET)
      (OPEN-BRACKET)
      (CLOSE-PAREN)
      (OPEN-PAREN)
      (COMMENT)
      (LITERAL)
      (QCONID)
      (QVARID)
      (QCONSYM)
      (QVARSYM)
      (TOPBLOCK)))
   'nil)
  "Table of lexical tokens.")

(defconst haskell-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((TOPBLOCK QVARSYM QCONSYM QVARID QCONID LITERAL COMMENT OPEN-PAREN CLOSE-PAREN OPEN-BRACKET CLOSE-BRACKET OPEN-BRACE CLOSE-BRACE COMMA SEMICOLON DOTDOT COLON DOUBLE-COLON EQUAL LAMBDA VERTICAL-BAR BACKQUOTE LEFT-ARROW RIGHT-ARROW AT TILDE DOUBLE-ARROW UNDERSCORE WHERE NEVER)
       nil
       (topdecl
        ((decl)))
       (decl
        ((gendecl))
        ((funlhs rhs))
        ((pat rhs)
         (wisent-raw-tag
          (semantic-tag-new-function $1 nil nil))))
       (decls
        ((decls decl))
        (nil))
       (gendecl
        ((NEVER)))
       (funlhs
        ((QVARID apat apats)
         (wisent-raw-tag
          (semantic-tag-new-function $1 nil nil)))
        ((OPEN-PAREN funlhs CLOSE-PAREN apat apats)
         (identity $2)))
       (apats
        ((apats apat))
        (nil))
       (apat
        ((QVARID))
        ((QVARID AT apat))
        ((gcon))
        ((qcon))
        ((qcon OPEN-BRACE fpats CLOSE-BRACE))
        ((LITERAL))
        ((UNDERSCORE))
        ((OPEN-PAREN pat CLOSE-PAREN))
        ((OPEN-PAREN comma_pats CLOSE-PAREN))
        ((OPEN-BRACKET comma_pats CLOSE-BRACKET))
        ((TILDE apat)))
       (comma_pats
        ((NEVER)))
       (pat
        ((apat qconop_apats))
        ((gcon apats))
        ((qcon apats)))
       (qconop_apats
        ((qconop_apats qconop apat))
        (nil))
       (gcon
        ((OPEN-PAREN CLOSE-PAREN))
        ((OPEN-BRACKET CLOSE-BRACKET))
        ((OPEN-PAREN commas CLOSE-PAREN))
        ((qcon)))
       (commas
        ((NEVER)))
       (qcon
        ((QCONID))
        ((OPEN-PAREN QCONSYM CLOSE-PAREN)))
       (qconop
        ((QCONSYM))
        ((BACKQUOTE QCONID BACKQUOTE)))
       (fpats1
        ((fpats1 COMMA fpat))
        ((fpat)))
       (fpats
        ((fpats1))
        (nil))
       (fpat
        ((QVARID EQUAL pat))
        ((QVARID)))
       (rhs
        ((EQUAL exp opt_where_decls))
        ((gdrhs opt_where_decls))
        (nil))
       (opt_where_decls
        ((WHERE decls))
        (nil))
       (gdrhs
        ((gd EQUAL exp gdrhs0)))
       (gdrhs0
        ((gdrhs))
        (nil))
       (gd
        ((VERTICAL-BAR exp0)))
       (exp
        ((NEVER)))
       (exp0
        ((NEVER))))
     '(topdecl)))
  "Parser table.")

(defun haskell-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table haskell-wy--parse-table
        semantic-debug-parser-source "haskell.wy"
        semantic-flex-keywords-obarray haskell-wy--keyword-table
        semantic-lex-types-obarray haskell-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;

;;; Epilogue
;;



(provide 'haskell-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; haskell-wy.el ends here
