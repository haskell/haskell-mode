;;; haskell-mode.el --- A Haskell editing mode    -*-coding: iso-8859-1;-*-

;; Copyright (C) 1992, 1997-1998 Simon Marlow, Graeme E Moss, and Tommy Thorn
;; Copyright (C) 2003 Free Software Foundation, Inc

;; Authors: 1992      Simon Marlow
;;          1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>,
;;          2001-2002 Reuben Thomas (>=v1.4)
;;          2003      Dave Love <fx@gnu.org>
;; Keywords: faces files Haskell
;; Version: 1.43
;; URL: http://www.haskell.org/haskell-mode/

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
;; To provide a pleasant mode to browse and edit Haskell files, linking
;; into the following supported modules:
;;
;; `haskell-font-lock', Graeme E Moss and Tommy Thorn
;;   Fontifies standard Haskell keywords, symbols, functions, etc.
;;
;; `haskell-decl-scan', Graeme E Moss
;;   Scans top-level declarations, and places them in a menu.
;;
;; `haskell-doc', Hans-Wolfgang Loidl
;;   Echoes types of functions or syntax of keywords when the cursor is idle.
;;
;; `haskell-indent', Guy Lapalme
;;   Intelligent semi-automatic indentation.
;;
;; `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
;;   Simple indentation.
;;
;; `haskell-hugs', Guy Lapalme
;;   Interaction with Hugs interpreter.
;;
;; `haskell-ghci', Chris Web
;;   Interaction with GHCi interpreter.
;;
;;
;; This mode supports full Haskell 1.4 including literate scripts.
;; In some versions of (X)Emacs it may only support Latin-1, not Unicode.
;;
;; Installation:
;; 
;; Put in your ~/.emacs:
;;
;;    (setq auto-mode-alist
;;          (append auto-mode-alist
;;                  '(("\\.[hg]s$"  . haskell-mode)
;;                    ("\\.hi$"     . haskell-mode)
;;                    ("\\.l[hg]s$" . literate-haskell-mode))))
;;
;;    (autoload 'haskell-mode "haskell-mode"
;;       "Major mode for editing Haskell scripts." t)
;;    (autoload 'literate-haskell-mode "haskell-mode"
;;       "Major mode for editing literate Haskell scripts." t)
;;
;; with `haskell-mode.el' accessible somewhere on the load-path.
;; To add a directory `~/lib/emacs' (for example) to the load-path,
;; add the following to .emacs:
;;
;;    (setq load-path (cons "~/lib/emacs" load-path))
;;
;; To turn any of the supported modules on for all buffers, add the
;; appropriate line(s) to .emacs:
;;
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;;    (add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
;;
;; Make sure the module files are also on the load-path.  Note that
;; the two indentation modules are mutually exclusive: Use only one.
;;
;;
;; Customisation:
;;
;; Set the value of `haskell-literate-default' to your preferred
;; literate style: 'bird or 'latex, within .emacs as follows:
;;
;;    (setq haskell-literate-default 'latex)
;;
;; Also see the customisations of the modules.
;;
;;
;; History:
;;
;; This mode is based on an editing mode by Simon Marlow 11/1/92
;; and heavily modified by Graeme E Moss and Tommy Thorn 7/11/98.
;; 
;; If you have any problems or suggestions specific to a supported
;; module, consult that module for a list of known bugs, and an
;; author to contact via email.  For general problems or suggestions,
;; consult the list below, then email gem@cs.york.ac.uk and
;; thorn@irisa.fr quoting the version of the mode you are using, the
;; version of emacs you are using, and a small example of the problem
;; or suggestion.
;;
;; Version 1.43:
;;   Various tweaks to doc strings and customization support from
;;   Ville Skyttä <scop@xemacs.org>.
;;
;; Version 1.42:
;;   Added autoload for GHCi inferior mode (thanks to Scott
;;   Williams for the bug report and fix).
;;
;; Version 1.41:
;;   Improved packaging, and made a couple more variables
;;   interactively settable.
;;
;; Version 1.4:
;;   Added GHCi mode from Chris Webb, and tidied up a little.
;;
;; Version 1.3:
;;   The literate or non-literate style of a buffer is now indicated
;;   by just the variable haskell-literate: nil, 'bird, or 'latex.
;;   For literate buffers with ambiguous style, the value of
;;   haskell-literate-default is used.
;;
;; Version 1.2:
;;   Separated off font locking, declaration scanning and simple
;;   indentation, and made them separate modules.  Modules can be
;;   added easily now.  Support for modules haskell-doc,
;;   haskell-indent, and haskell-hugs.  Literate and non-literate
;;   modes integrated into one mode, and literate buffer indicated by
;;   value of haskell-literate(-bird-style).
;;
;; Version 1.1:
;;   Added support for declaration scanning under XEmacs via
;;   func-menu.  Moved operators to level two fontification.
;;
;; Version 1.0:
;;   Added a nice indention support from Heribert Schuetz
;;   <Heribert.Schuetz@informatik.uni-muenchen.de>:
;;
;;     I have just hacked an Emacs Lisp function which you might prefer
;;     to `indent-relative' in haskell-mode.el.  See below.  It is not
;;     really Haskell-specific because it does not take into account
;;     keywords like `do', `of', and `let' (where the layout rule
;;     applies), but I already find it useful.
;;
;;   Cleaned up the imenu support.  Added support for literate scripts.
;;
;; Version 0.103 [HWL]:
;;   From Hans Wolfgang Loidl <hwloidl@dcs.gla.ac.uk>:
;;
;;   I (HWL) added imenu support by copying the appropriate functions
;;   from hugs-mode.  A menu-bar item "Declarations" is now added in
;;   haskell mode.  The new code, however, needs some clean-up.
;;
;; Version 0.102:
;;
;;   Moved C-c C-c key binding to comment-region.  Leave M-g M-g to do
;;   the work.  comment-start-skip is changed to comply with comment-start.
;;
;; Version 0.101:
;;
;;   Altered indent-line-function to indent-relative.
;;
;; Version 0.100:
;; 
;;   First official release.

;; Present Limitations/Future Work (contributions are most welcome!):
;;
;; . Would like RET in Bird-style literate mode to add a ">" at the
;;   start of a line when previous line starts with ">".  Or would
;;   "> " be better?
;;
;; . Support for GreenCard?
;;

;;; All functions/variables start with `(literate-)haskell-'.

;; Version of mode.
(defconst haskell-version "1.43"
  "`haskell-mode' version number.")
(defun haskell-version ()
  "Echo the current version of `haskell-mode' in the minibuffer."
  (interactive)
  (message "Using haskell-mode version %s" haskell-version))

(defgroup haskell nil
  "Major mode for editing Haskell programs."
  :group 'languages
  :prefix "haskell-")

;; Haskell-like function for creating [from..to].
(defun haskell-enum-from-to (from to)
  (if (> from to)
      ()
    (cons from (haskell-enum-from-to (1+ from) to))))

;; Set up autoloads for the modules we supply
(autoload 'turn-on-haskell-font-lock "haskell-font-lock"
  "Turn on Haskell font locking." t)
(autoload 'turn-on-haskell-decl-scan "haskell-decl-scan"
  "Turn on Haskell declaration scanning." t)
(autoload 'turn-on-haskell-doc-mode "haskell-doc"
  "Turn on Haskell Doc minor mode." t)
(autoload 'turn-on-haskell-indent "haskell-indent"
  "Turn on Haskell indentation." t)
(autoload 'turn-on-haskell-simple-indent "haskell-simple-indent"
  "Turn on simple Haskell indentation." t)
(autoload 'turn-on-haskell-hugs "haskell-hugs"
  "Turn on interaction with a Hugs interpreter." t)
(autoload 'turn-on-haskell-ghci "haskell-ghci"
  "Turn on interaction with a GHCi interpreter." t)

;; Are we running FSF Emacs or XEmacs?
(defvar haskell-running-xemacs
  (string-match "Lucid\\|XEmacs" emacs-version)
  "non-nil if we are running XEmacs, nil otherwise.")

;; Are we looking at a literate script?
(defvar haskell-literate nil
  "*If not nil, the current buffer contains a literate Haskell script.
Possible values are: 'bird and 'latex, for Bird-style and LaTeX-style
literate scripts respectively.  Set by `haskell-mode' and
`literate-haskell-mode'.  For an ambiguous literate buffer -- ie. does
not contain either \"\\begin{code}\" or \"\\end{code}\" on a line on
its own, nor does it contain \">\" at the start of a line -- the value
of `haskell-literate-default' is used.

Always buffer-local.")
(make-variable-buffer-local 'haskell-literate)
;; Default literate style for ambiguous literate buffers.
(defcustom haskell-literate-default 'bird
  "*Default value for `haskell-literate'.
Used if the style of a literate buffer is ambiguous.  This variable should
be set to the preferred literate style.  For example, place within
.emacs:

   (setq haskell-literate-default 'latex)"
  :type '(choice bird latex nil)
  :group 'haskell)

;; Mode maps.
(defvar haskell-mode-map (let ((keymap (make-sparse-keymap)))
			   (define-key keymap "\C-c\C-c" 'comment-region)
			   keymap)
  "Keymap used in Haskell mode.")

;; Syntax table.
(defvar haskell-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\'" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    (cond ((featurep 'xemacs)
	   ;; I don't know whether this is equivalent to the below
	   ;; (modulo nesting).  -- fx
	   (modify-syntax-entry ?{  "(}1" table)
	   (modify-syntax-entry ?}  "){4" table)
	   (modify-syntax-entry ?-  "_ 23" table))
	  (t
	   ;; The following get comment syntax right, similarly to C++
	   ;; (but the `b' style needs to be the other way round).  In
	   ;; Emacs 21, the `n' indicates that they nest.  Proper
	   ;; treatment of comments requires syntactic font-lock
	   ;; support.
	   (modify-syntax-entry ?\{  "(}1nb" table)
	   (modify-syntax-entry ?\}  "){4nb" table)
	   (modify-syntax-entry ?-  "_ 123" table)
	   (modify-syntax-entry ?\n ">" table)))

    (modify-syntax-entry ?\` "$`" table)
    (modify-syntax-entry ?\\ "\\" table)
    (mapcar (lambda (x)
	      (modify-syntax-entry x "_" table))
	    ;; Some of these are actually OK by default.
	    "!#$%&*+./:<=>?@^|~")
    (unless (featurep 'mule)
      ;; Non-ASCII syntax should be OK, at least in Emacs.
      (mapcar (lambda (x)
		(modify-syntax-entry x "_" table))
	      (concat (haskell-enum-from-to ?\241 ?\277)
		      "\327\367"))
      (mapcar (lambda (x)
		(modify-syntax-entry x "w" table))
	      (concat (haskell-enum-from-to ?\300 ?\326)
		      (haskell-enum-from-to ?\330 ?\337)
		      (haskell-enum-from-to ?\340 ?\366)
		      (haskell-enum-from-to ?\370 ?\377))))
    table)
  "Syntax table used in Haskell mode.")

;; Various mode variables.
(defun haskell-vars ()
  (kill-all-local-variables)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "-- ")
  (make-local-variable 'comment-padding)
  (setq comment-padding 0)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "[-{]- *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'haskell-comment-indent)
  (make-local-variable 'comment-end)
  (setq comment-end ""))

;; The main mode functions
;;;###autoload
(defun haskell-mode ()
  "Major mode for editing Haskell programs.  Last adapted for Haskell 1.4.
Blank lines separate paragraphs, comments start with `-- '.

\\<haskell-mode-map>\\[indent-for-comment] will place a comment at an appropriate place on the current line.
\\[comment-region] comments (or with prefix arg, uncomments) each line in the region.

Literate scripts are supported via `literate-haskell-mode'.  The
variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more
details.

Modules can hook in via `haskell-mode-hook'.  The following modules
are supported with an `autoload' command:

   `haskell-font-lock', Graeme E Moss and Tommy Thorn
     Fontifies standard Haskell keywords, symbols, functions, etc.

   `haskell-decl-scan', Graeme E Moss
     Scans top-level declarations, and places them in a menu.

   `haskell-doc', Hans-Wolfgang Loidl
     Echoes types of functions or syntax of keywords when the cursor is idle.

   `haskell-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `haskell-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

   `haskell-hugs', Guy Lapalme
     Interaction with Hugs interpreter.

Module X is activated using the command `turn-on-X'.  For example,
`haskell-font-lock' is activated using `turn-on-haskell-font-lock'.
For more information on a module, see the help for its `turn-on-X'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `haskell-doc' is irregular in using `turn-(on/off)-haskell-doc-mode'.)

Use `haskell-version' to find out what version this is.

Invokes `haskell-mode-hook' if not nil."

  (interactive)
  (haskell-mode-generic nil))

;;;###autoload
(defun literate-haskell-mode ()
  "As `haskell-mode' but for literate scripts."

  (interactive)
  (haskell-mode-generic
   (save-excursion
     (goto-char (point-min))
     (if (re-search-forward "^\\\\begin{code}$\\|^\\\\end{code}$"
                            (point-max) t)
         'latex
       (if (re-search-forward "^>" (point-max) t)
           'bird
         haskell-literate-default)))))

(defun haskell-mode-generic (literate)
  "Common part of `haskell-mode' and `literate-haskell-mode'.
Former calls this with LITERATE nil.  Latter calls with LITERATE `bird' or
`latex'."

  (haskell-vars)
  (setq major-mode 'haskell-mode)
  (setq mode-name "Haskell")
  (setq haskell-literate literate)
  (use-local-map haskell-mode-map)
  (set-syntax-table haskell-mode-syntax-table)
  (run-hooks 'haskell-mode-hook))

;; Find the indentation level for a comment.
(defun haskell-comment-indent ()
  (skip-chars-backward " \t")
  ;; if the line is blank, put the comment at the beginning,
  ;; else at comment-column
  (if (bolp) 0 (max (1+ (current-column)) comment-column)))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)$" . haskell-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.l[gh]s$" . literate-haskell-mode))
;;;###autoload(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
;;;###autoload(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;;;###autoload(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;;###autoload(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;; Provide ourselves:

(provide 'haskell-mode)

;;; haskell-mode.el ends here
