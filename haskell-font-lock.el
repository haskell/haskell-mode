;;; haskell-font-lock.el --- Font locking module for Haskell Mode

;; Copyright 1997-1998 Graeme E Moss, and Tommy Thorn
;; Copyright 2003 Free Software Foundation, Inc.

;; Authors: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>
;;          2003  Dave Love <fx@gnu.org>
;; Keywords: faces files Haskell
;; Version: 1.2

;;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Purpose:
;;
;; To support fontification of standard Haskell keywords, symbols,
;; functions, etc.  Supports full Haskell 1.4 as well as LaTeX- and
;; Bird-style literate scripts.
;;
;; Installation:
;; 
;; To turn font locking on for all Haskell buffers under the Haskell
;; mode of Moss&Thorn, add this to .emacs:
;;
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
;;
;; Otherwise, call `turn-on-haskell-font-lock'.
;;
;;
;; Customisation:
;;
;; The colours and level of font locking may be customised.  See the
;; documentation on `turn-on-haskell-font-lock' for more details.
;;
;;
;; History:
;;
;; If you have any problems or suggestions, after consulting the list
;; below, email gem@cs.york.ac.uk and thorn@irisa.fr quoting the
;; version of the mode you are using, the version of emacs you are
;; using, and a small example of the problem or suggestion.  Note that
;; this module requires a reasonably recent version of Emacs.  It
;; requires Emacs 21 to cope with Unicode characters and to do proper
;; syntactic fontification.
;;
;; Version 1.3:
;;   From Dave Love:
;;   Support for proper behaviour (including with Unicode identifiers)
;;   in Emacs 21 only hacked in messily to avoid disturbing the old
;;   stuff.  Needs integrating more cleanly.  Allow literate comment
;;   face to be customized.  Some support for fontifying definitions.
;;   (I'm not convinced the faces should be customizable -- fontlock
;;   faces are normally expected to be consistent.)
;;
;; Version 1.2:
;;   Added support for LaTeX-style literate scripts.  Allow whitespace
;;   after backslash to end a line for string continuations.
;;
;; Version 1.1:
;;   Use own syntax table.  Use backquote (neater).  Stop ''' being
;;   highlighted as quoted character.  Fixed `\"' fontification bug
;;   in comments.
;;
;; Version 1.0:
;;   Brought over from Haskell mode v1.1.
;;
;; Present Limitations/Future Work (contributions are most welcome!):
;;
;; . Debatable whether `()' `[]' `(->)' `(,)' `(,,)' etc. should be
;;   highlighted as constructors or not.  Should the `->' in
;;   `id :: a -> a' be considered a constructor or a keyword?  If so,
;;   how do we distinguish this from `\x -> x'?  What about the `\'?
;;
;; . XEmacs can support both `--' comments and `{- -}' comments
;;   simultaneously.  If XEmacs is detected, this should be used.
;; 
;; . Support for GreenCard?
;;

;;; All functions/variables start with
;;; `(turn-(on/off)-)haskell-font-lock' or `haskell-fl-'.

(eval-when-compile (require 'haskell-mode))
(require 'font-lock)

;; Version.
(defconst haskell-font-lock-version "1.3"
  "haskell-font-lock version number.")
(defun haskell-font-lock-version ()
  "Echo the current version of haskell-font-lock in the minibuffer."
  (interactive)
  (message "Using haskell-font-lock version %s" haskell-font-lock-version))

(defvar haskell-font-lock-keywords ()
  "The default definitions used by font lock for fontification of
non-literate Haskell scripts.  This variable is set by
`turn-on-haskell-font-lock' and then used by `font-lock-defaults'.")

(defvar haskell-font-lock-keywords-1 ()
  "Medium level font lock definitions for non-literate Haskell.")

(defvar haskell-font-lock-keywords-2 ()
  "High level font lock definitions for non-literate Haskell.")

(defvar bird-literate-haskell-font-lock-keywords ()
  "The default definitions used by font lock for fontification of
Bird-style literate Haskell scripts.  This variable is set by
`turn-on-haskell-font-lock' and then used by `font-lock-defaults'.")

(defvar bird-literate-haskell-font-lock-keywords-1 ()
  "Medium level font lock definitions for Bird-style literate Haskell.")

(defvar bird-literate-haskell-font-lock-keywords-2 ()
  "High level font lock definitions for Bird-style literate Haskell.")

(defvar latex-literate-haskell-font-lock-keywords ()
  "The default definitions used by font lock for fontification of
LaTeX-style literate Haskell scripts.  This variable is set by
`turn-on-haskell-font-lock' and then used by `font-lock-defaults'.")

(defvar latex-literate-haskell-font-lock-keywords-1 ()
  "Medium level font lock definitions for LaTeX-style literate Haskell.")

(defvar latex-literate-haskell-font-lock-keywords-2 ()
  "High level font lock definitions for LaTeX-style literate Haskell.")

(eval-and-compile
(defconst haskell-emacs21-features (string-match "[[:alpha:]]" "x")
  "Non-nil if we have regexp char classes.
Assume this means we have other useful features from Emacs 21.")

  (if haskell-emacs21-features
(defvar haskell-literate-comment-face 'haskell-comment-face
  "Face with which to fontify literate comments.
Set to `default' to avoid fontification of them."))

;; The font lock regular expressions.
  (if haskell-emacs21-features
(defun haskell-font-lock-keywords-create (bird-literate latex-literate level)
  "Creates appropriate LEVEL (1 or 2) of fontification definitions
for (BIRD-LITERATE or LATEX-LITERATE) Haskell scripts.  Returns keywords
suitable for `font-lock-keywords'."
  (let* (;; Bird-style literate scripts start a line of code with
	 ;; "^>", otherwise a line of code starts with "^".
	 (line-prefix (if bird-literate "^>" "^"))

	 ;; Most names are borrowed from the lexical syntax of the Haskell
	 ;; report. 

         ;; We allow _ as the first char to fit GHC
         (id "\\b[[:alnum:]'_]+\\b")
         (varid "\\b[[:lower:]_][[:alnum:]'_]*\\b")
         (conid "\\b[[:upper:]][[:alnum:]'_]*\\b")
	 (modid (concat "\\b" conid "\\(?:\\." conid "\\)*\\b"))
         (qconid (concat modid "\\." conid))
	 (sym
	  ;; Add backslash to the symbol-syntax chars.  This seems to
	  ;; be thrown for some reason by backslash's escape syntax.
	  "\\(?:\\s_\\|\\\\\\)+")
	 font-lock-keywords)

    (setq font-lock-keywords
	  `(
;;
;; NOTICE the ordering below is significant
;;
	    ("^#.*$" 0 'font-lock-warning-face t)
	    ,(eval-when-compile
	       (regexp-opt
		;; ?? `as' and `qualified' aren't in the Haskell98 list.
		;; `_' can go in here since it has temporary word syntax.
		'("as" "case" "class" "data" "default" "deriving" "do" "else"
		  "hiding" "if" "import" "in" "infix" "infixl" "infixr"
		  "instance" "let" "module" "newtype" "of" "qualified" "then"
		  "type" "where" "_")
		'words))
	    (,(eval-when-compile
		;; Would lose at bol.
		(concat "\\S_"
			(regexp-opt '(".." "::" "=" "\\" "|" "<-" "->"
				      "@" "~" "=>")
				    t)
			"\\S_"))
	     (1 'haskell-operator-face))

	    ;; These four are debatable...
	    ("()" 0 'haskell-constructor-face)
	    ("(,*)" 0 'haskell-constructor-face)
	    ("\\[\\]" 0 'haskell-constructor-face)
	    ("(->)" 0 'haskell-constructor-face)
	    ,`(,qconid 0 'haskell-constructor-face)
	    ,@(if (eq level 2)
		  `(,`(,(concat "\`" varid "\`") 0 'haskell-operator-face)))
	    ;; Expensive.
	    ,`(,conid 0 'haskell-constructor-face)
	    ,@(if (eq level 2)
		  (list (list (concat "\\S_\\(:\\(?:" sym "\\)?\\)")
			      '(1 'haskell-constructor-face))))
	    ,@(if (eq level 2)
		  (list (list sym '(0 'haskell-operator-face))))

	    ;; Declarations.
	    (,(concat line-prefix "\\(" varid "\\)\\s-*\\(?:" varid
		      "\\|::\\|=\\||\\)")
	     (1 'haskell-definition-face))
	    (,(concat line-prefix "\\(" varid "\\)\\s-*\\(" sym "\\)")
	     (1 'default)
	     (2 'haskell-definition-face))
	    (,(concat line-prefix "\\(" varid "\\)\\s-*\\s(")
	     (1 'haskell-definition-face))
	    (,(concat line-prefix "(\\(" sym "\\))")
	     (1 'haskell-definition-face))))
      (if bird-literate
	  ;; Don't colour the Bird tracks.
	  (setq font-lock-keywords
		(append font-lock-keywords '(("^>" 0 'default t)))))
      font-lock-keywords))
(defun haskell-font-lock-keywords-create (bird-literate latex-literate level)
  "Creates appropriate LEVEL (1 or 2) of fontification definitions
for (BIRD-LITERATE or LATEX-LITERATE) Haskell scripts.  Returns keywords
suitable for `font-lock-keywords'."
  (let* (;; Bird-style literate scripts start a line of code with
	 ;; "^>", otherwise a line of code starts with "^".
	 (line-prefix (if bird-literate "^>" "^"))

	 ;; Most names are borrowed from the lexical syntax of the Haskell
	 ;; report.
         ;; Some of these definitions have been superseded by using the
         ;; syntax table instead.

	 (ASCsymbol "-!#$%&*+./<=>?@\\\\^|~") 
         ;; Put the minus first to make it work in ranges.
         (ISOsymbol "\241-\277\327\367")
         (ISOlarge  "\300-\326\330-\337")
         (ISOsmall  "\340-\366\370-\377")
         (small
          (concat "a-z" ISOsmall))
         (large
          (concat "A-Z" ISOlarge))
         (symbol
          (concat ASCsymbol ISOsymbol))

         ;; We allow _ as the first char to fit GHC
         (id
          (concat "\\b\\([" small large "0-9'_]+\\)\\b"))
         (varid
          (concat "\\b\\([" small "_][" small large "0-9'_]*\\)\\b"))
         (conid
          (concat "\\b\\([" large "][" small large "0-9'_]*\\)\\b"))
	 (modid
	  (concat "\\b" conid "\\(\\." conid "\\)*\\b"))
         (qvarid
          (concat modid "\\." varid))
         (qconid
          (concat modid "\\." conid))
	 (sym
	  (concat "[" symbol ":]+"))

         ;; Reserved operations
         (reservedsym
          '(".." "::" "=" "\\" "|" "<-" "->" "@" "~" "=>"))
         ;; Reserved identifiers
         ;(reservedid
         ; '("as" "case" "class" "data" "default" "deriving" "do" "else"
         ;   "hiding" "if" "import" "in" "infix" "infixl" "infixr"
         ;   "instance" "let" "module" "newtype" "of" "qualified" "then"
         ;   "type" "where"))
	 ;; make-regexp applied to reservedid creates the following
	 ;; regexp
	 (reservedid
	  "\\b\\(c\\(ase\\|lass\\)\\|d\\(ata\\|e\\(fault\\|riving\\)\\|o\\)\\|else\\|hiding\\|i\\([fn]\\|mport\\|n\\(fix\\(\\|[lr]\\)\\|stance\\)\\)\\|let\\|module\\|newtype\\|of\\|qualified\\|t\\(hen\\|ype\\)\\|where\\)\\b")

         ;; This unreadable regexp matches strings and character
         ;; constants.  We need to do this with one regexp to handle
         ;; stuff like '"':"'".  The regexp is the composition of
         ;; "([^"\\]|\\.)*" for strings and '([^\\]|\\.[^']*)' for
         ;; characters, allowing for string continuations.
	 ;; Could probably be improved...
         (string-and-char
          (concat "\\(\\(\"\\|" line-prefix "[ \t]*\\\\\\)\\([^\"\\\\\n]\\|\\\\.\\)*\\(\"\\|\\\\[ \t]*$\\)\\|'\\([^'\\\\\n]\\|\\\\.[^'\n]*\\)'\\)"))

	 ;; Top-level declarations
	 ;; These are not included as they don't work well (yet).
;	 (topdecl1
;	  (concat line-prefix "\\(" varid "\\)\\(\\s-\\|::\\|=\\||\\)"))
;	 (topdecl2
;	  (concat line-prefix varid "\\s-*\\(" sym "\\)"))
;	 (topdecl3
;	  (concat line-prefix "(\\(" sym "\\))"))

	 font-lock-keywords)

    (setq font-lock-keywords
	  `(
;;
;; NOTICE the ordering below is significant
;;
	    ("--.*$" 0 'haskell-comment-face t)
	    ("^#.*$" 0 'font-lock-warning-face t)
	    ;; Expensive.
	    ,`(,string-and-char 1 'haskell-string-char-face)
	    ;; These four are debatable...
	    ("()" 0 'haskell-constructor-face)
	    ("(,*)" 0 'haskell-constructor-face)
	    ("\\[\\]" 0 'haskell-constructor-face)
	    ("(->)" 0 'haskell-constructor-face)
	    ;; Expensive.
	    ,`(,reservedid 1 'haskell-keyword-face)
	    ,`(,qvarid 0 'haskell-default-face)
	    ,`(,qconid 0 'haskell-constructor-face)
	    ,@(if (eq level 2)
		  `(,`(,(concat "\`" varid "\`") 0 'haskell-operator-face))
		'())
	    ;; Expensive.
	    ,`(,conid 1 'haskell-constructor-face)
	    ;; Very expensive.
	    ,`(,sym 0 ,`(let ((match (match-string 0)))
			  ,`(cond
			     ,`(,`(member match ',reservedsym)
				'haskell-operator-face)
			     ((eq (aref match 0) ?:) 'haskell-constructor-face)
			     ,@(if (eq level 2)
				   '((t 'haskell-operator-face))
				 '()))))
	   ;;	   (list topdecl1 1 ''haskell-definition-face t)
	   ;;	   (list topdecl2 1 ''haskell-definition-face t)
	   ;;	   (list topdecl3 1 ''haskell-definition-face t)
	   ))
      (if bird-literate
	  (setq font-lock-keywords
		`(("^[^>\n].*$" 0 'haskell-comment-face t)
		  ,@font-lock-keywords
		  ("^>" 0 'haskell-default-face t)))
        (if latex-literate
            (setq font-lock-keywords
                  `((haskell-fl-latex-comments 0 'font-lock-comment-face t)
                    ,@font-lock-keywords))))
      font-lock-keywords))))

;; The next three aren't used in Emacs 21.

(defvar haskell-fl-latex-cache-pos nil
  "Position of cache point used by `haskell-fl-latex-cache-in-comment'.
Should be at the start of a line.")

(defvar haskell-fl-latex-cache-in-comment nil
  "If `haskell-fl-latex-cache-pos' is outside a
\\begin{code}..\\end{code} block (and therefore inside a comment),
this variable is set to t, otherwise nil.")

(defun haskell-fl-latex-comments (end)
  "Sets `match-data' according to the region of the buffer before end
that should be commented under LaTeX-style literate scripts."
  (let ((start (point)))
    (if (= start end)
        ;; We're at the end.  No more to fontify.
        nil
      (if (not (eq start haskell-fl-latex-cache-pos))
          ;; If the start position is not cached, calculate the state
          ;; of the start.
          (progn
            (setq haskell-fl-latex-cache-pos start)
            ;; If the previous \begin{code} or \end{code} is a
            ;; \begin{code}, then start is not in a comment, otherwise
            ;; it is in a comment.
            (setq haskell-fl-latex-cache-in-comment
                  (if (and
                       (re-search-backward
                        "^\\(\\(\\\\begin{code}\\)\\|\\(\\\\end{code}\\)\\)$"
                        (point-min) t)
                       (match-end 2))
                      nil t))
            ;; Restore position.
            (goto-char start)))
      (if haskell-fl-latex-cache-in-comment
          (progn
            ;; If start is inside a comment, search for next \begin{code}.
            (re-search-forward "^\\\\begin{code}$" end 'move)
            ;; Mark start to end of \begin{code} (if present, till end
            ;; otherwise), as a comment.
            (set-match-data (list start (point)))
            ;; Return point, as a normal regexp would.
            (point))
        ;; If start is inside a code block, search for next \end{code}.
        (if (re-search-forward "^\\\\end{code}$" end t)
            ;; If one found, mark it as a comment, otherwise finish.
            (point))))))

(eval-and-compile
  (if haskell-emacs21-features
(defvar haskell-fl-syntax
  ;; The mode syntax table will basically DTRT.  However, it's
  ;; convenient to treat the non-ASCII punctuation characters as
  ;; symbol.  (We probably have to keep `,' and `;' as
  ;; punctuation, so we can't just consider sequences of
  ;; punctuation and symbol syntax.  We could also use
  ;; categories.)
  `((?_ . "w")				; in case _ has normal syntax
    (?' . "w")
    ,@(let (cs i lim)
	(let ((table (make-syntax-table)))
	  (map-char-table
	   (lambda (k v)
	     ;; The current Emacs 22 codebase can pass either a char
	     ;; or a char range.
	     (if (consp k)
		 (setq i (car k)
		       lim (cdr k))
	       (setq i k 
		     lim k))
	     (if (<= i lim)
		 (when (and (> i 127)
			    (equal v '(1)))
		   (push (cons i "-") cs))
	       (setq i (1+ i))))
	   (standard-syntax-table)))
	cs))  
  "Syntax required for font locking.
Given as a list of pairs for use in `font-lock-defaults'.")
(defvar haskell-fl-syntax
  ;; It's easier for us to manually set the ISO Latin1 syntax as I'm
  ;; not sure what libraries are available and how they differ from
  ;; Haskell, eg. the iso-syntax library of Emacs 19.34 defines \241
  ;; as punctuation for good reasons but this conflicts with Haskell
  ;; so we would have to redefine it.  It's simpler for us to set the
  ;; syntax table according to the Haskell report for all of the 8-bit
  ;; characters.
  `((?\  . " ")
    (?\t . " ")
    (?\" . " ")
    (?\' . "w")
    (?_  . "w")
    (?\( . "()")
    (?\) . ")(")
    (?[  . "(]")
    (?]  . ")[")
    (?{  . "(}1")
    (?}  . "){4")
    (?-  . "_ 23")
    (?\` . "$`")
    ,@(mapcar (lambda (x) (cons x "_"))
	      (concat "!#$%&*+./:<=>?@\\^|~" (haskell-enum-from-to ?\241 ?\277)
		      "\327\367"))
    ,@(mapcar (lambda (x) (cons x "w"))
	      (concat (haskell-enum-from-to ?\300 ?\326) (haskell-enum-from-to ?\330 ?\337)
		      (haskell-enum-from-to ?\340 ?\366) (haskell-enum-from-to ?\370 ?\377))))
  "Syntax required for font locking.  Given as a list of pairs for use
in `font-lock-defaults'.")))

(defconst haskell-basic-syntactic-keywords
  '(
    ;; Character constants (since apostrophe can't have string syntax)
    ("\\Sw\\('\\)\\([^\\']\\|\\\\[^']+\\|\\\\'\\)\\('\\)" (1 "|") (3 "|"))
    ;; Deal with instances of `--' which don't form a comment.
    ("\\s_\\{3,\\}" (0 (if (string-match "\\`-*\\'" (match-string 0))
			   nil		; Sequence of hyphens.  Do nothing in
					; case of things like `{---'.
			 "_")))))	; other symbol sequence

(defconst haskell-bird-syntactic-keywords
  (cons '("^\\([^\n>]\\)[^\n]*\\(\n\\)"  (1 "!") (2 "!"))
	haskell-basic-syntactic-keywords))

(defun haskell-fl-bobp (end)
  "Find first non-newline character at beginning of buffer."
  (if (= (point) 1)			; not bobp in case of narrowing
      (re-search-forward "[^\n]" nil t)))

(defconst haskell-latex-syntactic-keywords
  (append
   '(("^\\\\begin{code}\\(\n\\)" 1 "!")
     (haskell-fl-bobp (0 "!"))	       ; start comment at buffer start
     ("^\\(\\\\\\)end{code}$" 1 "!"))
   haskell-basic-syntactic-keywords))

(defun haskell-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Haskell."
  (if (nth 3 state)
      'font-lock-string-face		; as normal
    ;; Else comment.  If it's from syntax table, use default face.
    (if (eq 'syntax-table (nth 7 state))
	haskell-literate-comment-face
      'haskell-comment-face)))

(defun haskell-font-lock-defaults-create (bird-literate latex-literate)
  "Locally set `font-lock-defaults' for Haskell.
If BIRD-LITERATE is non-nil then the font locking is made
suitable for Bird-style literate Haskell scripts, and similarly for
LATEX-LITERATE and LaTeX-style literate Haskell scripts."
  (setq haskell-font-lock-keywords-1
	(haskell-font-lock-keywords-create nil nil 1))
  (setq haskell-font-lock-keywords-2
	(haskell-font-lock-keywords-create nil nil 2))
  (setq haskell-font-lock-keywords
	haskell-font-lock-keywords-1)
  (setq bird-literate-haskell-font-lock-keywords-1
	(haskell-font-lock-keywords-create t nil 1))
  (setq bird-literate-haskell-font-lock-keywords-2
	(haskell-font-lock-keywords-create t nil 2))
  (setq bird-literate-haskell-font-lock-keywords
	bird-literate-haskell-font-lock-keywords-1)
  (setq latex-literate-haskell-font-lock-keywords-1
	(haskell-font-lock-keywords-create nil t 1))
  (setq latex-literate-haskell-font-lock-keywords-2
	(haskell-font-lock-keywords-create nil t 2))
  (setq latex-literate-haskell-font-lock-keywords
	latex-literate-haskell-font-lock-keywords-1)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	(if bird-literate
	    (append (list '(bird-literate-haskell-font-lock-keywords
			    bird-literate-haskell-font-lock-keywords-1
			    bird-literate-haskell-font-lock-keywords-2)
			  nil nil haskell-fl-syntax)
		    (if haskell-emacs21-features
			(list nil
			      (cons 'font-lock-syntactic-keywords
				    haskell-bird-syntactic-keywords)
			      '(font-lock-syntactic-face-function
				. haskell-syntactic-face-function))))
          (if latex-literate
	      (append
	       (list '(latex-literate-haskell-font-lock-keywords
		       latex-literate-haskell-font-lock-keywords-1
		       latex-literate-haskell-font-lock-keywords-2)
		     nil nil haskell-fl-syntax)
	       (if haskell-emacs21-features
		   (list nil
			 (cons 'font-lock-syntactic-keywords
			       haskell-latex-syntactic-keywords)
			 '(font-lock-syntactic-face-function
			   . haskell-syntactic-face-function))))
            (append
	     (list '(haskell-font-lock-keywords
		     haskell-font-lock-keywords-1
		     haskell-font-lock-keywords-2)
		   nil nil haskell-fl-syntax)
	     (if haskell-emacs21-features
		 (list nil
		       (cons 'font-lock-syntactic-keywords
			     haskell-basic-syntactic-keywords))))))))

;; Faces required for font locking.
(defun haskell-fl-faces ()
  "Defines faces required for Haskell font locking."
  ;; XEmacs does not have a simple function for making the faces but
  ;; makes them when `require'd which was done by
  ;; turn-on-haskell-font-lock, so we don't need to explicitly make
  ;; them for XEmacs, and in fact we shouldn't as an error will be
  ;; produced.
  (if (fboundp 'font-lock-make-faces) (font-lock-make-faces))
  (copy-face 'font-lock-keyword-face 'haskell-keyword-face)
  (copy-face 'font-lock-type-face 'haskell-constructor-face)
  (copy-face 'font-lock-string-face 'haskell-string-char-face)
  (copy-face 'font-lock-function-name-face 'haskell-operator-face)
  (copy-face 'font-lock-comment-face 'haskell-comment-face)
  (copy-face 'default 'haskell-default-face)
  (copy-face 'font-lock-variable-name-face 'haskell-definition-face)
  )

;; The main functions.
(defun turn-on-haskell-font-lock ()
  "Turns on font locking in current buffer for Haskell 1.4 scripts.

Changes the current buffer's `font-lock-defaults', and adds the
following faces:

   `haskell-keyword-face'      for reserved keywords and syntax,
   `haskell-constructor-face'  for data- and type-constructors, class names,
                               and module names,
   `haskell-string-char-face'  for strings and characters,
   `haskell-operator-face'     for symbolic and alphanumeric operators,
   `haskell-comment-face'      for comments, and
   `haskell-default-face'      for ordinary code.

The faces are initialised to the following font lock defaults:

   `haskell-keyword-face'      `font-lock-keyword-face'
   `haskell-constructor-face'  `font-lock-type-face'
   `haskell-string-char-face'  `font-lock-string-face'
   `haskell-operator-face'     `font-lock-function-name-face'
   `haskell-comment-face'      `font-lock-comment-face'
   `haskell-default-face'      <default face>

Two levels of fontification are defined: level one (the default)
and level two (more colour).  The former does not colour operators.
Use the variable `font-lock-maximum-decoration' to choose
non-default levels of fontification.  For example, adding this to
.emacs:

  (setq font-lock-maximum-decoration '((haskell-mode . 2) (t . 0)))

uses level two fontification for `haskell-mode' and default level for
all other modes.  See documentation on this variable for further
details.

To alter an attribute of a face, add a hook.  For example, to change
the foreground colour of comments to brown, add the following line to
.emacs:

  (add-hook 'haskell-font-lock-hook
      (lambda ()
          (set-face-foreground 'haskell-comment-face \"brown\")))

Note that the colours available vary from system to system.  To see
what colours are available on your system, call
`list-colors-display' from emacs.

To turn font locking on for all Haskell buffers, add this to .emacs:

  (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)

To turn font locking on for the current buffer, call
`turn-on-haskell-font-lock'.  To turn font locking off in the current
buffer, call `turn-off-haskell-font-lock'.

Bird-style literate Haskell scripts are supported: If the value of
`haskell-literate-bird-style' (automatically set by the Haskell mode
of Moss&Thorn) is non-nil, a Bird-style literate script is assumed.

Invokes `haskell-font-lock-hook' if not nil.

Use `haskell-font-lock-version' to find out what version this is."

  (interactive)
  (require 'font-lock)
  (haskell-fl-faces)
  (let ((literate (if (boundp 'haskell-literate) haskell-literate)))
    (haskell-font-lock-defaults-create (eq literate 'bird)
                                       (eq literate 'latex)))
  ;; Get help from font-lock-syntactic-keywords.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (run-hooks 'haskell-font-lock-hook)
  (turn-on-font-lock))

(defun turn-off-haskell-font-lock ()
  "Turns off font locking in current buffer."
  (interactive)
  (if (and (boundp 'font-lock-mode) font-lock-mode)
      (font-lock-mode -1)))

;;; Provide ourselves:

(provide 'haskell-font-lock)

;;; haskell-font-lock ends here.
