;;; haskell-font-lock.el --- Font locking module for Haskell Mode

;; Copyright 2003, 2004  Free Software Foundation, Inc.
;; Copyright 1997-1998 Graeme E Moss, and Tommy Thorn

;; Authors: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>
;;          2003  Dave Love <fx@gnu.org>
;; Keywords: faces files Haskell
;; Version: 1.4

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

(eval-when-compile
  (require 'haskell-mode)
  (require 'cl))
(require 'font-lock)

;; Version.
(defconst haskell-font-lock-version "1.4"
  "haskell-font-lock version number.")
(defun haskell-font-lock-version ()
  "Echo the current version of haskell-font-lock in the minibuffer."
  (interactive)
  (message "Using haskell-font-lock version %s" haskell-font-lock-version))

(defcustom haskell-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.
This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises w.r.t layout.
If t, try to use whichever font is available.  Othrwise you can
set it to a particular font of your preference among `japanese-jisx0208'
and `unicode'."
  :group 'haskell
  :type '(choice (const nil)
	         (const t)
	         (const unicode)
	         (const japanese-jisx0208)))

(defconst haskell-font-lock-symbols-alist
  (append
   ;; The symbols can come from a JIS0208 font.
   (and (fboundp 'make-char) (charsetp 'japanese-jisx0208)
	(memq haskell-font-lock-symbols '(t japanese-jisx0208))
	(list (cons "\\" (make-char 'japanese-jisx0208 38 75))
	      (cons "->" (make-char 'japanese-jisx0208 34 42))
	      (cons "<-" (make-char 'japanese-jisx0208 34 43))
	      (cons "=>" (make-char 'japanese-jisx0208 34 77))))
   ;; Or a unicode font.
   (and (fboundp 'decode-char)
	(memq haskell-font-lock-symbols '(t unicode))
	(list (cons "\\" (decode-char 'ucs 955))
	      (cons "->" (decode-char 'ucs 8594))
	      (cons "<-" (decode-char 'ucs 8592))
	      (cons "=>" (decode-char 'ucs 8658))
	      (cons "::" (decode-char 'ucs 8759))))))

;; Use new vars for the font-lock faces.  The indirection allows people to
;; use different faces than in other modes, as before.
(defvar haskell-keyword-face 'font-lock-keyword-face)
(defvar haskell-constructor-face 'font-lock-type-face)
(defvar haskell-string-char-face 'font-lock-string-face)
;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defvar haskell-definition-face 'font-lock-function-name-face)
;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `haskell-definition-face'.
(defvar haskell-operator-face 'font-lock-variable-name-face)
(defvar haskell-literate-comment-face 'font-lock-doc-face
  "Face with which to fontify literate comments.
Set to `default' to avoid fontification of them.")

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

(defconst haskell-emacs21-features (string-match "[[:alpha:]]" "x")
  "Non-nil if we have regexp char classes.
Assume this means we have other useful features from Emacs 21.")

(defun haskell-font-lock-symbols-keywords ()
  (when (and (fboundp 'compose-region))
    (let ((alist nil))
      (dolist (x haskell-font-lock-symbols-alist)
	(when (and (if (fboundp 'char-displayable-p)
		       (char-displayable-p (cdr x))
				    t)
		   (not (assoc (car x) alist)))    ;Not yet in alist.
	    (push x alist)))
      (when alist
	`((,(concat "\\S_" (regexp-opt (mapcar 'car alist) t) "\\S_")
	   (1 (if (memq (get-text-property (- (point) 2) 'face)
			'(font-lock-doc-face font-lock-string-face
			  font-lock-comment-face))
		  nil
		(compose-region
		 (match-beginning 1) (match-end 1)
		 (cdr (assoc (match-string 1) ',alist)))
		nil))))))))

;; The font lock regular expressions.
(defun haskell-font-lock-keywords-create (literate level)
  "Create fontification definitions for Haskell scripts.
Returns keywords suitable for `font-lock-keywords'."
  (let* (;; Bird-style literate scripts start a line of code with
	 ;; "^>", otherwise a line of code starts with "^".
	 (line-prefix (if (eq literate 'bird) "^> ?" "^"))

	 ;; Most names are borrowed from the lexical syntax of the Haskell
	 ;; report.
         ;; Some of these definitions have been superseded by using the
         ;; syntax table instead.

	 ;; (ASCsymbol "-!#$%&*+./<=>?@\\\\^|~")
         ;; Put the minus first to make it work in ranges.
         ;; (ISOsymbol "\241-\277\327\367")
         (ISOlarge  "\300-\326\330-\337")
         (ISOsmall  "\340-\366\370-\377")
         (small
          (if haskell-emacs21-features "[:lower:]" (concat "a-z" ISOsmall)))
         (large
          (if haskell-emacs21-features "[:upper:]" (concat "A-Z" ISOlarge)))
	 (alnum
	  (if haskell-emacs21-features "[:alnum:]" (concat small large "0-9")))
         ;; (symbol
         ;;  (concat ASCsymbol ISOsymbol))

         ;; We allow _ as the first char to fit GHC
         (varid (concat "\\b[" small "_][" alnum "'_]*\\b"))
         (conid (concat "\\b[" large "][" alnum "'_]*\\b"))
	 (modid (concat "\\b" conid "\\(\\." conid "\\)*\\b"))
         (qvarid (concat modid "\\." varid))
         (qconid (concat modid "\\." conid))
	 (sym
	  ;; We used to use the below for non-Emacs21, but I think the
	  ;; regexp based on syntax works for other emacsen as well.  -- Stef
	  ;; (concat "[" symbol ":]+")
	  ;; Add backslash to the symbol-syntax chars.  This seems to
	  ;; be thrown for some reason by backslash's escape syntax.
	  "\\(\\s_\\|\\\\\\)+")

         ;; Reserved operations
         (reservedsym
	  (concat "\\S_"
		  ;; (regexp-opt '(".." "::" "=" "\\" "|" "<-" "->"
		  ;; 		"@" "~" "=>") t)
		  "\\(->\\|\\.\\.\\|::\\|<-\\|=>\\|[=@\\|~]\\)"
		  "\\S_"))
         ;; Reserved identifiers
	 (reservedid
	  (concat "\\b"
		  ;; ?? `as' and `qualified' aren't in the Haskell98 list.
		  ;; `_' can go in here since it has temporary word syntax.
		  ;; (regexp-opt
		  ;;  '("as" "case" "class" "data" "default" "deriving" "do"
		  ;;    "else" "hiding" "if" "import" "in" "infix" "infixl"
		  ;;    "infixr" "instance" "let" "module" "newtype" "of"
		  ;;    "qualified" "then" "type" "where" "_") t)
		  "\\(_\\|as\\|c\\(ase\\|lass\\)\\|d\\(ata\\|e\\(fault\\|riving\\)\\|o\\)\\|else\\|hiding\\|i\\(mport\\|n\\(fix[lr]?\\|stance\\)\\|[fn]\\)\\|let\\|module\\|newtype\\|of\\|qualified\\|t\\(hen\\|ype\\)\\|where\\)"
		  "\\b"))

         ;; This unreadable regexp matches strings and character
         ;; constants.  We need to do this with one regexp to handle
         ;; stuff like '"':"'".  The regexp is the composition of
         ;; "([^"\\]|\\.)*" for strings and '([^\\]|\\.[^']*)' for
         ;; characters, allowing for string continuations.
	 ;; Could probably be improved...
         (string-and-char
          (concat "\\(\\(\"\\|" line-prefix "[ \t]*\\\\\\)\\([^\"\\\\\n]\\|\\\\.\\)*\\(\"\\|\\\\[ \t]*$\\)\\|'\\([^'\\\\\n]\\|\\\\.[^'\n]*\\)'\\)"))

	 ;; Top-level declarations
	 (topdecl-var
	  (concat line-prefix "\\(" varid "\\)\\s-*\\("
		  varid "\\|" conid "\\|::\\|=\\||\\|\\s(\\)"))
	 (topdecl-var2
	  (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
	 (topdecl-sym
	  (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
	 (topdecl-sym2 (concat line-prefix "(\\(" sym "\\))"))

	 keywords)

    (setq keywords
	  `(
;;
;; NOTICE the ordering below is significant
;;
	    ;; Comments are highlighted using syntax tables.  --Stef
	    ;; ("--.*$" 0 haskell-comment-face t)
	    ("^#.*$" 0 'font-lock-warning-face t)
	    ,@(unless haskell-emacs21-features
	    ;; Expensive.
		`((,string-and-char 1 haskell-string-char-face)))

	    (,reservedid 1 haskell-keyword-face)
	    (,reservedsym 1 haskell-operator-face)

	    ;; Toplevel Declarations.
	    ;; Place them *before* generic id-and-op highlighting.
	    (,topdecl-var (1 haskell-definition-face))
	    (,topdecl-var2 (2 haskell-definition-face))
	    (,topdecl-sym (2 haskell-definition-face))
	    (,topdecl-sym2 (1 haskell-definition-face))

	    ;; These four are debatable...
	    ("(\\(,*\\|->\\))" 0 haskell-constructor-face)
	    ("\\[\\]" 0 haskell-constructor-face)
	    ;; Expensive.
	    (,qvarid 0 haskell-default-face)
	    (,qconid 0 haskell-constructor-face)
	    ,@(if (eq level 2)
		  `(,`(,(concat "\`" varid "\`") 0 haskell-operator-face)))
	    ;; Expensive.
	    (,conid 0 haskell-constructor-face)

	    ;; Very expensive.
	    (,sym 0 (if (eq (char-after (match-beginning 0)) ?:)
			haskell-constructor-face
		      ,@(if (eq level 2) '(haskell-operator-face))))
	    ,@(haskell-font-lock-symbols-keywords)))
    (unless haskell-emacs21-features
      (case literate
	(bird
	 (setq keywords
	       `(("^[^>\n].*$" 0 haskell-comment-face t)
		 ,@keywords
		 ("^>" 0 haskell-default-face t))))
	(latex
	 (setq keywords
                  `((haskell-fl-latex-comments 0 'font-lock-comment-face t)
		 ,@keywords)))))
    keywords))

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

(defvar haskell-fl-syntax
  (if haskell-emacs21-features
  ;; The mode syntax table will basically DTRT.  However, it's
  ;; convenient to treat the non-ASCII punctuation characters as
  ;; symbol.  (We probably have to keep `,' and `;' as
  ;; punctuation, so we can't just consider sequences of
  ;; punctuation and symbol syntax.  We could also use
  ;; categories.)
  `((?_ . "w")				; in case _ has normal syntax
    (?' . "w")
    ,@(let (cs i lim)
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
	     ;; This should probably be haskell's syntax-table instead.
	     (standard-syntax-table))
	cs))  
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
			(haskell-enum-from-to ?\340 ?\366) (haskell-enum-from-to ?\370 ?\377)))))
  "Syntax required for font locking.  Given as a list of pairs for use
in `font-lock-defaults'.")

(defconst haskell-basic-syntactic-keywords
  '(;; Character constants (since apostrophe can't have string syntax)
    ("\\Sw\\('\\)\\([^\\']\\|\\\\.\\)+\\('\\)" (1 "|") (3 "|"))
    ;; The \ is not escaping in \(x,y) -> x + y.
    ("\\(\\\\\\)(" (1 "."))
    ;; Deal with instances of `--' which don't form a comment.
    ("\\s_\\{3,\\}" (0 (if (string-match "\\`-*\\'" (match-string 0))
			   nil		; Sequence of hyphens.  Do nothing in
					; case of things like `{---'.
			 "_")))))	; other symbol sequence

(defconst haskell-bird-syntactic-keywords
  (cons '("^[^\n>]"  (0 "<"))
	haskell-basic-syntactic-keywords))

(defconst haskell-latex-syntactic-keywords
  (append
   '(("^\\\\begin{code}\\(\n\\)" 1 "!")
     ;; Note: buffer is widened during font-locking.
     ("\\`\\(.\\|\n\\)" (1 "!"))	       ; start comment at buffer start
     ("^\\(\\\\\\)end{code}$" 1 "!"))
   haskell-basic-syntactic-keywords))

(defun haskell-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Haskell."
  (cond
   ((nth 3 state) font-lock-string-face)		; as normal
    ;; Else comment.  If it's from syntax table, use default face.
   ((or (eq 'syntax-table (nth 7 state))
	(and (eq haskell-literate 'bird)
	     (memq (char-before (nth 8 state)) '(nil ?\n))))
    haskell-literate-comment-face)
   (t font-lock-comment-face)))

(defun haskell-font-lock-defaults-create (literate)
  "Locally set `font-lock-defaults' for Haskell.
If LITERATE is non-nil then the font locking is made
suitable for literate Haskell scripts, using Bird-style if LITERATE
is `bird' and LaTeX-style if it is `latex'."
  (setq haskell-font-lock-keywords-1
	(haskell-font-lock-keywords-create nil 1))
  (setq haskell-font-lock-keywords-2
	(haskell-font-lock-keywords-create nil 2))
  (setq haskell-font-lock-keywords
	haskell-font-lock-keywords-1)
  (setq bird-literate-haskell-font-lock-keywords-1
	(haskell-font-lock-keywords-create 'bird 1))
  (setq bird-literate-haskell-font-lock-keywords-2
	(haskell-font-lock-keywords-create 'bird 2))
  (setq bird-literate-haskell-font-lock-keywords
	bird-literate-haskell-font-lock-keywords-1)
  (setq latex-literate-haskell-font-lock-keywords-1
	(haskell-font-lock-keywords-create 'latex 1))
  (setq latex-literate-haskell-font-lock-keywords-2
	(haskell-font-lock-keywords-create 'latex 2))
  (setq latex-literate-haskell-font-lock-keywords
	latex-literate-haskell-font-lock-keywords-1)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	(list
	 (case literate
	  (bird
	   '(bird-literate-haskell-font-lock-keywords
			    bird-literate-haskell-font-lock-keywords-1
	     bird-literate-haskell-font-lock-keywords-2))
	  (latex
	   '(latex-literate-haskell-font-lock-keywords
		       latex-literate-haskell-font-lock-keywords-1
	     latex-literate-haskell-font-lock-keywords-2))
	  (t '(haskell-font-lock-keywords
	       haskell-font-lock-keywords-1
	       haskell-font-lock-keywords-2)))
	 nil nil haskell-fl-syntax nil
			 (cons 'font-lock-syntactic-keywords
	       (case literate
		(bird haskell-bird-syntactic-keywords)
		(latex haskell-latex-syntactic-keywords)
		(t haskell-basic-syntactic-keywords)))
			 '(font-lock-syntactic-face-function
			   . haskell-syntactic-face-function))))

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
   `haskell-default-face'      for ordinary code.

The faces are initialised to the following font lock defaults:

   `haskell-keyword-face'      `font-lock-keyword-face'
   `haskell-constructor-face'  `font-lock-type-face'
   `haskell-string-char-face'  `font-lock-string-face'
   `haskell-operator-face'     `font-lock-function-name-face'
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
  (let ((literate (if (boundp 'haskell-literate) haskell-literate)))
    (haskell-font-lock-defaults-create literate))
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
