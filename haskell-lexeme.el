;;; haskell-lexeme.el --- haskell lexical tokens   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2015 Gracjan Polak

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

(require 'rx)

(unless (category-docstring ?P)
  (define-category ?P "Haskell symbol constituent characters")
  (map-char-table
   #'(lambda (key val)
       (if (or
            (and (consp key) (> (car key) 128))
            (and (numberp key) (> key 128)))
           (if (member val '(Pc Pd Po Sm Sc Sk So))
	       (modify-category-entry key ?P))))
   unicode-category-table)

  (dolist (key (string-to-list "!#$%&*+./<=>?@^|~\\-"))
    (modify-category-entry key ?P)))

(defconst haskell-lexeme-modid
  "[[:upper:]][[:alnum:]'_]*"
  "Regexp matching a valid Haskell module identifier.

Note that GHC accepts Unicode category UppercaseLetter as a first
character. Following letters are from Unicode categories
UppercaseLetter, LowercaseLetter, OtherLetter, TitlecaseLetter,
ModifierLetter, DecimalNumber, OtherNumber, backslash or
underscore.

Note that this differs from constructor identifier as the latter
one can have any number of hash character at the end to
accommodate MagicHash extension.")

(defconst haskell-lexeme-id
  "[[:alpha:]_][[:alnum:]'_]*#*"
  "Regexp matching a valid Haskell identifier.

GHC accepts a string starting with any alphabetic character or
underscore followed by any alphanumeric character or underscore
or apostrophe.")

(defconst haskell-lexeme-sym
  "\\(:?\\cP\\|:\\)+"
  "Regexp matching a valid Haskell variable or constructor symbol.

GHC accepts a string of chars from the set
[:!#$%&*+./<=>?@^|~\\-] or Unicode category Symbol for chars with
codes larger than 128 only.")

(defconst haskell-lexeme-modid-opt-prefix
  (concat "\\(?:" haskell-lexeme-modid "\\.\\)*")
  "Regexp matching a valid Haskell module prefix, potentially empty.

Module path prefix is separated by dots and finishes with a
dot. For path component syntax see `haskell-lexeme-modid'.")

(defconst haskell-lexeme-qid-or-qsym
  (rx-to-string `(: (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (| (regexp ,haskell-lexeme-id) (regexp ,haskell-lexeme-sym)
                              ))))
  "Regexp matching a valid qualified identifier or symbol.

Note that (match-string 1) returns the unqualified part.")

(defconst haskell-lexeme-qid
  (rx-to-string `(: (regexp "'*")
                    (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (regexp ,haskell-lexeme-id))))
  "Regexp matching a valid qualified identifier.

Note that (match-string 1) returns the unqualified part.")

(defconst haskell-lexeme-qsym
  (rx-to-string `(: (regexp "'*")
                    (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (regexp ,haskell-lexeme-id))))
  "Regexp matching a valid qualified symbol.

Note that (match-string 1) returns the unqualified part.")

(defconst haskell-lexeme-number
  (rx (| (: (regexp "[0-9]+\\.[0-9]+") (opt (regexp "[eE][-+]?[0-9]+")))
         (regexp "[0-9]+[eE][-+]?[0-9]+")
         (regexp "0[xX][0-9a-fA-F]+")
         (regexp "0[oO][0-7]+")
         (regexp "[0-9]+")))
  "Regexp matching a floating point, decimal, octal or hexadecimal number.

Note that negative sign char is not part of a number.")

(defconst haskell-lexeme-char-literal-inside
  (rx (| (regexp "[^\n'\\]")
         (: "\\"
            (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'"
               "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
               "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
               "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
               "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
               (: "^" (regexp "[]A-Z@^_\\[]"))))))
  "Regexp matching an inside of a character literal.")

(defconst haskell-lexeme-char-literal
  (rx-to-string `(: "'" (regexp ,haskell-lexeme-char-literal-inside) "'"))
  "Regexp matching a character literal.")

(defconst haskell-lexeme-string-literal-inside
  (rx (* (| (regexp "[^\n\"\\]")
            (: "\\"
               (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'" "&"
                  "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
                  "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
                  "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
                  "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
                  (: "^" (regexp "[]A-Z@^_\\[]"))
                  (regexp "[ \t\n\r\v\f]*\\\\"))))))
  "Regexp matching an inside of a string literal.")

(defconst haskell-lexeme-string-literal
  (rx-to-string `(: "\"" (regexp ,haskell-lexeme-string-literal-inside) "\""))
  "Regexp matching a string literal.")

(defun haskell-lexeme-classify-by-first-char (char)
  "Classify token by CHAR.

CHAR is a chararacter that is assumed to be first character of a token."
  (let ((category (get-char-code-property char 'general-category)))

    (cond
     ((or (member char '(?! ?# ?$ ?% ?& ?* ?+ ?. ?/ ?< ?= ?> ?? ?@ ?^ ?| ?~ ?\\ ?-))
          (and (> char 127)
               (member category '(Pc Pd Po Sm Sc Sk So))))
      'varsym)
     ((equal char ?:)
      'consym)
     ((equal char ?\')
      'char)
     ((equal char ?\")
      'string)
     ((member category '(Lu Lt))
      'conid)
     ((or (equal char ?_)
          (member category '(Ll Lo)))
      'varsym)
     ((and (>= char ?0) (<= char 9))
      'number)
     ((member char '(?\] ?\[ ?\( ?\) ?\{ ?\} ?\` ?\, ?\;))
      'special))))

(defun haskell-lexeme-looking-at-token ()
  "Like `looking-at' but understands Haskell lexemes.

Moves point forward over whitespace.  Returns a symbol describing
type of Haskell token recognized.  Use `match-string',
`match-beginning' and `match-end' with argument 0 to query match
result.

Possible results are:
- 'special: for chars [](){}`,;
- 'comment: for single line comments
- 'nested-comment: for multiline comments
- 'qsymid: for qualified identifiers or symbols
- 'string: for strings literals
- 'char: for char literals
- 'decimal: for decimal, float, hexadecimal and octal number literals
- 'template-haskell-quote: for a string of apostrophes for template haskell

Note that for qualified symbols (match-string 1) returns the
unqualified identifier or symbol.  Further qualification for
symbol or identifier can be done with:

   (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))

See `haskell-lexeme-classify-by-first-char' for details."
  (skip-syntax-forward "->")
  (let
      ((case-fold-search nil)
       (point (point-marker)))
    (or
     (and (looking-at "{-")
          (progn
            (save-excursion
              (forward-comment 1)
              (set-match-data (list point (point-marker))))
            'nested-comment))
     (and (looking-at haskell-lexeme-char-literal)
          'char)
     (and (looking-at haskell-lexeme-string-literal)
          'string)
     (and (looking-at "[][(){}`,;]")
          'special)
     (and (looking-at haskell-lexeme-qid-or-qsym)
          (if (and (eq (- (match-end 0) (match-beginning 0)) 2)
                   (equal (match-string 0) "--"))
              (progn
                (set-match-data (list point (set-marker (make-marker) (line-end-position))))
                'comment)
            'qsymid))
     (and (looking-at haskell-lexeme-number)
          'number)
     (and (looking-at "'+")
          'template-haskell-quote))))

(provide 'haskell-lexeme)

;;; haskell-lexeme.el ends here
