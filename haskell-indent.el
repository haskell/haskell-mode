;; haskell-indent.el --- "semi-intelligent" indentation module for Haskell Mode

;; Copyright 2004  Free Software Foundation, Inc.
;; Copyright 1997-1998  Guy Lapalme

;; Author: 1997-1998 Guy Lapalme <lapalme@iro.umontreal.ca>

;; Keywords: indentation Haskell layout-rule
;; Version: 1.2
;; URL: http://www.iro.umontreal.ca/~lapalme/layout/index.html

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
;; To support automatic indentation of Haskell programs using
;; the layout rule descrived in section 1.5 and appendix B.3 of the
;; the Haskell report.  The rationale and the implementation principles
;; are described in an article to appear in Journal of Functional Programming.
;;   "Dynamic tabbing for automatic indentation with the layout rule"
;;   
;; It supports literate scripts.
;; Haskell indentation is performed
;;     within \begin{code}...\end{code} sections of a literate script
;;     and in lines beginning with > with Bird style literate script
;; TAB aligns to the left column outside of these sections.
;;
;; Installation:
;; 
;; To turn indentation on for all Haskell buffers under the Haskell
;; mode of Moss&Thorn <http://www.haskell.org/haskell-mode/>
;; add this to .emacs:
;;
;;    (add-hook haskell-mode-hook 'turn-on-haskell-indent)
;;
;; Otherwise, call `turn-on-haskell-indent'.
;;
;;
;; Customisation:
;;       The "standard" offset for statements is 4 spaces.
;;       It can be changed by setting the variable "haskell-indent-offset" to
;;       another value
;;
;;       The default number of blanks after > in a Bird style literate script
;;       is 1; it can be changed by setting the variable
;;       "haskell-indent-literate-Bird-default-offset"
;;
;;       `haskell-indent-hook' is invoked if not nil.
;;
;;; All functions/variables start with
;;; `(turn-(on/off)-)haskell-indent' or `haskell-indent-'.

;; This file can also be used as a hook for the Hugs Mode developed by
;;         Chris Van Humbeeck <chris.vanhumbeeck@cs.kuleuven.ac.be>
;; It can be obtained at:
;; http://www-i2.informatik.rwth-aachen.de/Forschung/FP/Haskell/hugs-mode.el
;;
;; For  the Hugs mode put the following in your .emacs
;; 
;;(setq auto-mode-alist (append auto-mode-alist '(("\\.hs$" . hugs-mode))))
;;(autoload 'hugs-mode "hugs-mode" "Go into hugs mode" t)
;;
;; If only the indentation mode is used then replace the two
;; preceding lines with
;;(setq auto-mode-alist (append auto-mode-alist
;;                              '(("\\.hs$" . turn-on-haskell-indent))))
;;(autoload 'turn-on-haskell-indent "hindent" "Indentation mode for Haskell" t)
;;
;; For indentation in both cases then add the following to your .emacs
;;(setq hugs-mode-hook
;;      '(lambda ()
;;         (setq indent-line-function 'haskell-indent-cycle)
;;         (define-key hugs-mode-map "\r" 'newline)
;;         (define-key hugs-mode-map "\t" 'haskell-indent-cycle)
;;         (define-key hugs-mode-map "\C-c=" 'haskell-indent-insert-equal)
;;         (define-key hugs-mode-map "\C-c|" 'haskell-indent-insert-guard)
;;         (define-key hugs-mode-map "\C-co" 'haskell-indent-insert-otherwise)
;;         (define-key hugs-mode-map "\C-cw" 'haskell-indent-insert-where)
;;         (define-key hugs-mode-map "\C-c." 'haskell-indent-align-guards-and-rhs)))
;;(autoload 'haskell-indent-cycle "hindent" "Indentation cycle for Haskell" t)
;;

(eval-when-compile (require 'cl))	;need defs of push and pop

;;; Code:
(defgroup haskell-indent nil
  "Haskell indentation."
  :group 'haskell
  :prefix "haskell-indent-")

(defcustom haskell-indent-offset 4
  "*Indentation of Haskell statements with respect to containing block."
  :type 'integer
  :group 'haskell-indent)

(defcustom haskell-indent-literate-Bird-default-offset 1
  "*Default number of blanks after > in a Bird style literate script."
  :type 'integer
  :group 'haskell-indent)

(defcustom haskell-indent-rhs-align-column 0 
  "*Column on which to align right-hand sides (use 0 for ad-hoc alignment)." 
  :type 'integer 
  :group 'haskell-indent) 

(defsubst haskell-indent-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst haskell-indent-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defun haskell-indent-point-to-col (apoint)
  "Return the column number of APOINT."
  (save-excursion
    (goto-char apoint)
    (current-column)))

(defconst haskell-indent-start-keywords-re 
  (concat "\\<\\("
	  "class\\|data\\|i\\(mport\\|n\\(fix\\(\\|[lr]\\)\\|stance\\)\\)\\|"
	  "module\\|newtype\\|primitive\\|type"
	  "\\)\\>")
  "Regexp describing keywords to complete when standing at the first word
of a line.")

(defconst haskell-running-xemacs
  (string-match "Lucid\\|XEmacs" emacs-version)
  "t when using XEmacs or Lucid.")

;;; customizations for different kinds of environments
;;; in which dealing with low-level events are different
(if haskell-running-xemacs
    (progn ;;; for XEmacs
      (fset 'event-basic-type 'event-key)
      (fset 'read-event 'next-command-event)
      (defun haskell-indent-mark-active ()
	(if zmacs-regions
	    zmacs-region-active-p
	  t)))
  ;; in Gnu  Emacs
  (defun haskell-indent-mark-active ()
    mark-active)
  )

;;  for pushing indentation information

;; FIXME: should be renamed to haskell-indent-info.
(defvar indent-info)			;Used with dynamic scoping.

(defun haskell-indent-push-col (col &optional name)
  "Pushes indentation information for the column COL
followed by NAME (if present). Makes sure that the same indentation info
is not pushed twice in a row. Uses free var `indent-info'."
  (let ((tmp (cons col name)))
    (if (and indent-info (equal tmp (car indent-info)))
	indent-info
      (push tmp indent-info))))

(defun haskell-indent-push-pos (pos &optional name)
  "Pushes indentation information for the column corresponding to POS
followed by NAME (if present). "
  (haskell-indent-push-col (haskell-indent-point-to-col pos) name))

(defun haskell-indent-push-pos-offset (pos &optional offset)
  "Pushes indentation information for the column corresponding to POS
followed by an OFFSET (if present use its value otherwise use
`haskell-indent-offset')."
  (haskell-indent-push-col (+ (haskell-indent-point-to-col pos)
                              (or offset haskell-indent-offset))))

;;; redefinition of some Emacs function for dealing with
;;; Bird Style literate scripts 

(defun haskell-indent-bolp ()
  "`bolp' but dealing with Bird-style literate scripts."
  (or (bolp)
      (and (eq haskell-literate 'bird)
           (<= (current-column) (1+ haskell-indent-literate-Bird-default-offset))
           (eq (save-excursion (beginning-of-line) (following-char)) ?\>))
      )
  )

(defun haskell-indent-empty-line-p ()
  "Checks if the current line is empty; deals with Bird style scripts."
  (save-excursion
    (beginning-of-line)
    (if (and (eq haskell-literate 'bird)
             (eq (following-char) ?\>))
        (forward-char 1))
    (looking-at "[ \t]*$"))
  )

(defun haskell-indent-back-to-indentation ()
  "`back-to-indentation' function but dealing with Bird-style literate
scripts."
  (if (eq haskell-literate 'bird)
      (progn
        (beginning-of-line)
        (if (and (not (eolp)) (eq (following-char) ?\>))
            (progn
              (forward-char 1)
              (if (not (eolp))
                (skip-chars-forward " \t" (haskell-indent-get-end-of-line))))
          (back-to-indentation)))
    (back-to-indentation))
  )

(defun haskell-indent-current-indentation ()
  "`current-indentation' function but dealing with Bird-style literate
scripts."
  (if (eq haskell-literate 'bird)
      (save-excursion
        (haskell-indent-back-to-indentation)
        (current-column))
    (current-indentation))
  )

(defun haskell-indent-backward-to-indentation (n)
  "`backward-to-indentation' function but dealing with Bird-style literate
scripts."
  (if (eq haskell-literate 'bird)
      (progn
        (forward-line (- n)) 
        (haskell-indent-back-to-indentation))
    (backward-to-indentation n))
  )

(defun haskell-indent-forward-line (&optional n)
  "`forward-line' function but dealing with Bird-style literate scripts."
  (prog1
      (forward-line n)
    (if (and (eq haskell-literate 'bird) (eq (following-char) ?\>))
        (progn (forward-char 1)                ; skip > and initial blanks...
               (skip-chars-forward " \t"))))
  )

(defun haskell-indent-line-to (n)
  "`indent-line-to' function but dealing with Bird-style literate scripts."
  (if (eq haskell-literate 'bird)
      (progn
        (beginning-of-line)
        (if (eq (following-char) ?\>)
            (delete-char 1))
        (delete-horizontal-space)       ; remove any starting TABs so
        (indent-line-to n)              ; that indent-line only adds spaces
        (save-excursion
          (beginning-of-line)
          (if (> n 0) (delete-char 1))  ; delete the first space begore
          (insert ?\>)))                ; inserting a >
    (indent-line-to n))
  )

(defun haskell-indent-skip-blanks-and-newlines-forward (end)
  "Skips forward blanks, tabs and newlines until END taking
account of Bird style literate scripts."
  (skip-chars-forward " \t\n" end)
  (if (eq haskell-literate 'bird)
      (while (and (bolp) (eq (following-char) ?\>))
        (forward-char 1)                ; skip >
        (skip-chars-forward " \t\n" end)))
)

(defun haskell-indent-skip-blanks-and-newlines-backward (start)
  "Skips backward blanks, tabs and newlines upto START
taking account of Bird style literate scripts."
  (skip-chars-backward " \t\n" start)
  (if (eq haskell-literate 'bird)
      (while (and (eq (current-column) 1)
                  (eq (preceding-char) ?\>))
        (forward-char -1)               ; skip back >
        (skip-chars-backward " \t\n" start)))
)

;; specific functions for literate code

(defun haskell-indent-within-literate-code ()
  "Checks if point is within a part of literate Haskell code and if so
returns its start otherwise returns NIL:
If it is Bird Style, then returns the position of the >
otherwise returns the ending position \\begin{code}."
  (save-excursion
    (if (eq haskell-literate 'bird)
        (progn
          (beginning-of-line)
          (if (or (eq (following-char) ?\>)
                  (and (bolp) (forward-line -1) (eq (following-char) ?\>)))
              (progn
                (while (and (zerop (forward-line -1))
                            (eq (following-char) ?\>)))
                (if (not (eq (following-char) ?\>))
                    (forward-line))
                (point))))
      ;;  Look for a \begin{code} or \end{code} line. 
      (if (eq haskell-literate 'latex)
          (if (re-search-backward
               "^\\(\\\\begin{code}$\\)\\|\\(\\\\end{code}$\\)" nil t)
              ;; within a literate code part if it was a \\begin{code}.
              (match-end 1))
        (error "haskell-indent-within-literate-code: should not happen!")
        )))
  )

(defun haskell-indent-put-region-in-literate (beg end &optional arg)
  "Put lines of the region as a piece of literate code.
With C-u prefix arg, remove indication that the region is literate code.
It deals with both Bird style and non Bird-style scripts."
  (interactive "r\nP")
  (if haskell-literate
      (if (eq haskell-literate 'bird)
          (let ((comment-start "> "); change dynamic bindings for 
                (comment-end ""))   ; comment-region
            (comment-region beg end arg))
        ;; not Bird style
        (if (consp arg)                 ; remove the literate indication
            (save-excursion
              (goto-char end)           ; remove end
              (beginning-of-line)
              (if (looking-at "\\\\end{code}")
                  (kill-line 1)
                (forward-line -1)       ; perhaps the end is at the start of
                (if (looking-at "\\\\end{code}") ; of the previous line
                  (kill-line 1))
                )
              (goto-char beg)           ; remove end
              (beginning-of-line)
              (if (looking-at "\\\\begin{code}")
                  (kill-line 1))
              )
          (save-excursion               ; add the literate indication
            (goto-char end)
            (insert "\\end{code}\n")
            (goto-char beg)
            (insert "\\begin{code}\n")))
        )
    (error "Cannot put a region in literate in a non literate script"))    
  )

;;; Start of indentation code

(defun haskell-indent-start-of-def ()
  "Return the position of the start of a definition.
It is at the first character which is not in a comment after nearest
preceding non-empty line."
  (save-excursion
    (let (start-code
          (save-point (point)))
      ;; determine the starting point of the current piece of code
      (if (setq start-code (and haskell-literate
                            (haskell-indent-within-literate-code)))
          (setq start-code (1+ start-code))
        (setq start-code (point-min)))
      ;; go backward until the first preceding empty line
      (haskell-indent-forward-line -1)
      (while (and (not (haskell-indent-empty-line-p))
                  (> (point) start-code)
                  (= 0 (haskell-indent-forward-line -1))))
      ;; go forward after the empty line
      (if (haskell-indent-empty-line-p)
          (haskell-indent-forward-line 1))
      (setq start-code (point))
      ;; find the first line of code which is not a comment
      (forward-comment (point-max))
      (if (> (point) save-point)
	  start-code
	(point)))))

(defun haskell-indent-open-structure (start end)
  "If any structure (list or tuple) is not closed, between START and END,
returns the location of the opening symbol, nil otherwise."
  (save-excursion
    (nth 1 (parse-partial-sexp start end))))

(defun haskell-indent-in-string (start end)
  "If a string is not closed , between START and END, returns the
location of the opening symbol, nil otherwise."
  (save-excursion
    (let ((pps (parse-partial-sexp start end)))
      (if (nth 3 pps) (nth 8 pps)))))

(defun haskell-indent-in-comment (start end)
  "Check, starting from START, if END is at or within a comment.
Returns the location of the start of the comment, nil otherwise."
  (let (pps)
    (assert (<= start end))
    (cond ((= start end) nil)
	  ((nth 4 (save-excursion (setq pps (parse-partial-sexp start end))))
	   (nth 8 pps))
	  ;; We also want to say that we are *at* the beginning of a comment.
	  ((and (not (nth 8 pps))
		(nth 4 (save-excursion
			 (setq pps (parse-partial-sexp end (+ end 2))))))
	   (nth 8 pps)))))

(defvar haskell-indent-off-side-keywords-re
      "\\<\\(do\\|let\\|of\\|where\\)\\>[ \t]*")

(defun haskell-indent-type-at-point ()
  "Return the type of the line (also puts information in `match-data')."
  (cond
   ((haskell-indent-empty-line-p) 'empty)
   ((haskell-indent-in-comment (point-min) (point)) 'comment)
   ((looking-at "\\(\\([a-zA-Z]\\sw*\\)\\|_\\)[ \t\n]*") 'ident)
   ((looking-at "\\(|[^|]\\)[ \t\n]*") 'guard)
   ((looking-at "\\(=[^>=]\\|::\\|->\\|<-\\)[ \t\n]*") 'rhs)
   (t 'other)))

(defvar haskell-indent-current-line-first-ident ""
  "Global variable that keeps track of the first ident of the line to indent.")


(defun haskell-indent-contour-line (start end)
  "Generate contour information between START and END points."
  (if (< start end)
      (save-excursion
	(goto-char end)
	(haskell-indent-skip-blanks-and-newlines-backward start)
        (let ((cur-col (current-column))            ; maximum column number
              (fl 0)     ; number of lines that forward-line could not advance
              contour)
          (while (and (> cur-col 0) (= fl 0) (>= (point) start))
            (haskell-indent-back-to-indentation)
	    (if (< (point) start) (goto-char start))
            (and (not (member (haskell-indent-type-at-point)
                              '(empty comment))) ; skip empty and comment lines
                 (< (current-column) cur-col) ; less indented column found
                 (push (point) contour) ; new contour point found
                 (setq cur-col (current-column)))
            (setq fl (haskell-indent-forward-line -1))
            )
          contour))))

(defun haskell-indent-next-symbol (end)
  "Puts point to the next following symbol."
  (while (and (looking-at "\\s)")       ;skip closing parentheses
              (< (point) end))
    (forward-char 1))
  (if (< (point) end)
     (progn
       (forward-sexp 1)                      ; this skips also {- comments !!!
       (haskell-indent-skip-blanks-and-newlines-forward end))
    ))

(defun haskell-indent-separate-valdef (start end)
  "Returns a list of positions for important parts of a valdef."
  (save-excursion
    (let (valname valname-string aft-valname
                  guard aft-guard
                  rhs-sign aft-rhs-sign
                  type)
      ;; "parse" a valdef separating important parts
      (goto-char start)
      (setq type (haskell-indent-type-at-point))
      (if (or (eq type 'ident) (eq type 'other)) ; possible start of a value def
          (progn
            (if (eq type 'ident)
                (progn
                  (setq valname (match-beginning 0))
                  (setq valname-string (buffer-substring (match-beginning 0)
                                                         (match-end 0)))
                  (goto-char (match-end 0)))
              (skip-chars-forward " \t" end)
              (setq valname (point))    ; type = other
              (haskell-indent-next-symbol end))
            (while (and (< (point) end)
                        (setq type (haskell-indent-type-at-point))
                        (or (eq type 'ident) (eq type 'other)))
              (if (null aft-valname)
                  (setq aft-valname (point)))
              (haskell-indent-next-symbol end))))
      (if (and (< (point) end) (eq type 'guard)) ; start of a guard
          (progn
            (setq guard (match-beginning 0))
            (goto-char (match-end 0))
            (while (and (< (point) end)
                        (setq type (haskell-indent-type-at-point))
                        (not (eq type 'rhs)))
              (if (null aft-guard)
                  (setq aft-guard (point)))
              (haskell-indent-next-symbol end))))
      (if (and (< (point) end) (eq type 'rhs)) ; start of a rhs
          (progn
            (setq rhs-sign (match-beginning 0))
            (goto-char (match-end 0))
            (if (< (point) end)
                (setq aft-rhs-sign (point)))))
      (list valname valname-string aft-valname
            guard aft-guard rhs-sign aft-rhs-sign))))    

(defsubst haskell-indent-no-otherwise (guard)
  "Check if there is no otherwise at GUARD."
  (save-excursion
    (goto-char guard)
    (not (looking-at "|[ \t]*otherwise\\>"))))


(defun haskell-indent-guard (start end end-visible indent-info)
  "Finds indentation information for a line starting with a guard."
  (save-excursion
    (let* ((sep (haskell-indent-separate-valdef start end))
           (valname (nth 0 sep))
           (guard (nth 3 sep))
           (rhs-sign (nth 5 sep)))
      ;; push information indentation for the visible part
      (if (and guard (< guard end-visible) (haskell-indent-no-otherwise guard))
          (haskell-indent-push-pos guard)
        (if rhs-sign
            (haskell-indent-push-pos rhs-sign) ; probably within a data definition...
          (if valname
              (haskell-indent-push-pos-offset valname))))))
  indent-info)

(defun haskell-indent-rhs (start end end-visible indent-info)
  "Finds indentation information for a line starting with a rhs."
  (save-excursion
    (let* ((sep (haskell-indent-separate-valdef start end))
           (valname (nth 0 sep))
           (guard (nth 3 sep))
           (rhs-sign (nth 5 sep)))
      ;; push information indentation for the visible part
      (if (and rhs-sign (< rhs-sign end-visible))
          (haskell-indent-push-pos rhs-sign)
        (if (and guard (< guard end-visible))
            (haskell-indent-push-pos-offset guard)
          (if valname                   ; always visible !!
              (haskell-indent-push-pos-offset valname))))))
  indent-info)

(defconst haskell-indent-decision-table
  (let ((or "\\)\\|\\("))
    (concat "\\("
            "1.1.11" or                 ; 1= vn gd rh arh
            "1.1.10" or                 ; 2= vn gd rh
            "1.1100" or                 ; 3= vn gd agd
            "1.1000" or                 ; 4= vn gd
            "1.0011" or                 ; 5= vn rh arh
            "1.0010" or                 ; 6= vn rh
            "110000" or                 ; 7= vn avn
            "100000" or                 ; 8= vn
            "001.11" or                 ; 9= gd rh arh
            "001.10" or                 ;10= gd rh
            "001100" or                 ;11= gd agd
            "001000" or                 ;12= gd
            "000011" or                 ;13= rh arh
            "000010" or                 ;14= rh
            "000000"                    ;15= 
            "\\)")))

(defun haskell-indent-find-case (test)
  "Find the index that matches in the decision table."
  (if (string-match haskell-indent-decision-table test)
      ;; use the fact that the resulting match-data is a list of the form
      ;; (0 6 [2*(n-1) nil] 0 6) where n is the number of the matching regexp
      ;; so n= ((length match-data)/2)-1
      (- (/ (length (match-data)) 2) 1)
    (error "haskell-indent-find-case: impossible case: %s" test)
    ))

(defun haskell-indent-empty (start end end-visible indent-info)
  "Finds indentation points for an empty line."
  (save-excursion
    (let*
        ((sep (haskell-indent-separate-valdef start end))
         (valname (pop sep))
         (valname-string (pop sep))
         (aft-valname (pop sep))
         (guard (pop sep))
         (aft-guard (pop sep))
         (rhs-sign (pop sep))
         (aft-rhs-sign (pop sep))
         (last-line (= end end-visible))
         (test (string
                (if valname ?1 ?0)
                (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                (if (and guard (< guard end-visible)) ?1 ?0)
                (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0)))
         )
      (if (and valname-string           ; special case for start keywords
               (string-match haskell-indent-start-keywords-re valname-string))
          (progn
            (haskell-indent-push-pos valname)
            ;; very special for data keyword
            (if (string-match "\\<data\\>" valname-string)
                (if rhs-sign (haskell-indent-push-pos rhs-sign)
                  (haskell-indent-push-pos-offset valname))
              (haskell-indent-push-pos-offset valname)
              ))
        (case                           ; general case
            (haskell-indent-find-case test)
          ;; "1.1.11"   1= vn gd rh arh
          (1 (haskell-indent-push-pos valname)
             (haskell-indent-push-pos valname valname-string)
             (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
             (haskell-indent-push-pos aft-rhs-sign))
          ;; "1.1.10"   2= vn gd rh
          (2 (haskell-indent-push-pos valname)
             (haskell-indent-push-pos valname valname-string)
             (if last-line
                 (haskell-indent-push-pos-offset guard)
               (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))))
          ;; "1.1100"   3= vn gd agd
          (3 (haskell-indent-push-pos valname)
             (haskell-indent-push-pos aft-guard)
             (if last-line (haskell-indent-push-pos-offset valname)))
          ;; "1.1000"   4= vn gd
          (4 (haskell-indent-push-pos valname)
             (if last-line (haskell-indent-push-pos-offset guard 2)))
          ;; "1.0011"   5= vn rh arh
          (5 (haskell-indent-push-pos valname)
             (if (or (and aft-valname (= (char-after rhs-sign) ?\=))
                     (= (char-after rhs-sign) ?\:))
                 (haskell-indent-push-pos valname valname-string))
             (haskell-indent-push-pos aft-rhs-sign))
          ;; "1.0010"   6= vn rh
          (6 (haskell-indent-push-pos valname)
             (haskell-indent-push-pos valname valname-string)
             (if last-line (haskell-indent-push-pos-offset valname)))
          ;; "110000"   7= vn avn
          (7 (haskell-indent-push-pos valname)
             (if last-line
                 (haskell-indent-push-pos aft-valname)
               (haskell-indent-push-pos valname valname-string)))
          ;; "100000"   8= vn
          (8 (haskell-indent-push-pos valname))
          ;; "001.11"   9= gd rh arh
          (9 (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
             (haskell-indent-push-pos aft-rhs-sign))
          ;; "001.10"  10= gd rh
          (10 (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
	      (if last-line (haskell-indent-push-pos-offset guard)))
          ;; "001100"  11= gd agd
          (11 (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
	      (haskell-indent-push-pos aft-guard))
          ;; "001000"  12= gd
          (12 (if (haskell-indent-no-otherwise guard) (haskell-indent-push-pos guard "| "))
	      (if last-line (haskell-indent-push-pos-offset guard 2)))
          ;; "000011"  13= rh arh
          (13 (haskell-indent-push-pos aft-rhs-sign))
          ;; "000010"  14= rh
          (14 (if last-line (haskell-indent-push-pos-offset rhs-sign 2 )))
          ;; "000000"  15= 
          (t (error "haskell-indent-empty: %s impossible case" test ))))))
  indent-info)

(defun haskell-indent-ident (start end end-visible indent-info)
  "Finds indentation points for a line starting with an identifier."
  (save-excursion
    (let*
        ((sep (haskell-indent-separate-valdef start end))
         (valname (pop sep))
         (valname-string (pop sep))
         (aft-valname (pop sep))
         (guard (pop sep))
         (aft-guard (pop sep))
         (rhs-sign (pop sep))
         (aft-rhs-sign (pop sep))
         (last-line (= end end-visible))
         (is-where
          (string-match "where[ \t]*" haskell-indent-current-line-first-ident))
         (diff-first   ; not a function def with the same name
          (not(string= valname-string haskell-indent-current-line-first-ident)))
;         (is-type-def
;          (and rhs-sign (eq (char-after rhs-sign) ?\:)))
         (test (string
                (if valname ?1 ?0)
                (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                (if (and guard (< guard end-visible)) ?1 ?0)
                (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0)))
         )
      (if (and valname-string           ; special case for start keywords
               (string-match haskell-indent-start-keywords-re valname-string))
          (progn
            (haskell-indent-push-pos valname)
            (if (string-match "\\<data\\>" valname-string)
                ;; very special for data keyword
                (if aft-rhs-sign (haskell-indent-push-pos aft-rhs-sign)
                  (haskell-indent-push-pos-offset valname))
              (if (not (string-match
                        haskell-indent-start-keywords-re
                        haskell-indent-current-line-first-ident))
                  (haskell-indent-push-pos-offset valname))
              ))
        (if (string= haskell-indent-current-line-first-ident "::")
            (if valname (haskell-indent-push-pos valname))
          (case                         ; general case
              (haskell-indent-find-case test)
            ;; "1.1.11"   1= vn gd rh arh
            (1 (if is-where
                   (haskell-indent-push-pos guard)
                 (haskell-indent-push-pos valname)
                 (if diff-first (haskell-indent-push-pos aft-rhs-sign))))
            ;; "1.1.10"   2= vn gd rh
            (2 (if is-where
                   (haskell-indent-push-pos guard)
                 (haskell-indent-push-pos valname)
                 (if last-line
                     (haskell-indent-push-pos-offset guard))))
            ;; "1.1100"   3= vn gd agd
            (3 (if is-where
                   (haskell-indent-push-pos-offset guard)
                 (haskell-indent-push-pos valname)
                 (if diff-first
                     (haskell-indent-push-pos aft-guard))))
            ;; "1.1000"   4= vn gd
            (4 (if is-where
                   (haskell-indent-push-pos guard)
                 (haskell-indent-push-pos valname)
                 (if last-line
                     (haskell-indent-push-pos-offset guard 2))))
            ;; "1.0011"   5= vn rh arh
            (5 (if is-where
                   (haskell-indent-push-pos-offset valname)
                 (haskell-indent-push-pos valname) 
                 (if diff-first
                     (haskell-indent-push-pos aft-rhs-sign))))
            ;; "1.0010"   6= vn rh
            (6 (if is-where
                   (haskell-indent-push-pos-offset valname)
                 (haskell-indent-push-pos valname)
                 (if last-line
                     (haskell-indent-push-pos-offset valname))))
            ;; "110000"   7= vn avn
            (7 (if is-where
                   (haskell-indent-push-pos-offset valname)
                 (haskell-indent-push-pos valname)
                 (if last-line
                     (haskell-indent-push-pos aft-valname))))
            ;; "100000"   8= vn
            (8 (if is-where
                   (haskell-indent-push-pos-offset valname)
                 (haskell-indent-push-pos valname)))
            ;; "001.11"   9= gd rh arh
            (9 (if is-where
                   (haskell-indent-push-pos guard)
                 (haskell-indent-push-pos aft-rhs-sign)))
            ;; "001.10"  10= gd rh
            (10 (if is-where
                    (haskell-indent-push-pos guard)
                  (if last-line
                      (haskell-indent-push-pos-offset guard))))
            ;; "001100"  11= gd agd
            (11 (if is-where
                    (haskell-indent-push-pos guard)
                  (if (haskell-indent-no-otherwise guard)
                      (haskell-indent-push-pos aft-guard))))
            ;; "001000"  12= gd
            (12 (if last-line (haskell-indent-push-pos-offset guard 2)))
            ;; "000011"  13= rh arh
            (13 (haskell-indent-push-pos aft-rhs-sign))
            ;; "000010"  14= rh
            (14 (if last-line (haskell-indent-push-pos-offset rhs-sign 2)))
            ;; "000000"  15= 
            (t (error "haskell-indent-ident: %s impossible case" test )))))))
  indent-info)

(defun haskell-indent-other (start end end-visible indent-info)
  "Finds indentation points for a non-empty line starting with something other
than an identifier, a guard or rhs."
  (save-excursion
    (let*
        ((sep (haskell-indent-separate-valdef start end))
         (valname (pop sep))
         (valname-string (pop sep))
         (aft-valname (pop sep))
         (guard (pop sep))
         (aft-guard (pop sep))
         (rhs-sign (pop sep))
         (aft-rhs-sign (pop sep))
         (last-line (= end end-visible))
         (test (string
                (if valname ?1 ?0)
                (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                (if (and guard (< guard end-visible)) ?1 ?0)
                (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0)))
         )
      (if (and valname-string           ; special case for start keywords
               (string-match haskell-indent-start-keywords-re valname-string))
          (haskell-indent-push-pos-offset valname)
        (case                           ; general case
         (haskell-indent-find-case test)
         ;; "1.1.11"   1= vn gd rh arh
         (1 (haskell-indent-push-pos aft-rhs-sign))
         ;; "1.1.10"   2= vn gd rh
         (2 (if last-line
                   (haskell-indent-push-pos-offset guard)
               (haskell-indent-push-pos-offset rhs-sign 2)))
         ;; "1.1100"   3= vn gd agd
         (3 (haskell-indent-push-pos aft-guard))
         ;; "1.1000"   4= vn gd
         (4 (haskell-indent-push-pos-offset guard 2))
         ;; "1.0011"   5= vn rh arh
         (5 (haskell-indent-push-pos valname)
            (haskell-indent-push-pos aft-rhs-sign))
         ;; "1.0010"   6= vn rh
         (6 (if last-line
                (haskell-indent-push-pos-offset valname)
              (haskell-indent-push-pos-offset rhs-sign 2)))
         ;; "110000"   7= vn avn
         (7 (haskell-indent-push-pos-offset aft-valname))
         ;; "100000"   8= vn
         (8 (haskell-indent-push-pos valname))
         ;; "001.11"   9= gd rh arh
         (9 (haskell-indent-push-pos aft-rhs-sign))
         ;; "001.10"  10= gd rh
         (10 (if last-line
                   (haskell-indent-push-pos-offset guard)
               (haskell-indent-push-pos-offset rhs-sign 2)))
         ;; "001100"  11= gd agd
         (11 (if (haskell-indent-no-otherwise guard)
                   (haskell-indent-push-pos aft-guard)))
         ;; "001000"  12= gd
         (12 (if last-line (haskell-indent-push-pos-offset guard 2)))
         ;; "000011"  13= rh arh
         (13 (haskell-indent-push-pos aft-rhs-sign))
         ;; "000010"  14= rh
         (14 (if last-line (haskell-indent-push-pos-offset rhs-sign 2)))
         ;; "000000"  15= 
         (t (error "haskell-indent-other: %s impossible case" test ))))))
  indent-info)

(defun haskell-indent-valdef-indentation (start end end-visible curr-line-type
                                      indent-info)
  "Find indentation information for a value definition."
  (if (< start end-visible)
      (case curr-line-type
            (empty (haskell-indent-empty start end end-visible indent-info))
            (ident (haskell-indent-ident start end end-visible indent-info))
            (guard (haskell-indent-guard start end end-visible indent-info))
            (rhs   (haskell-indent-rhs start end end-visible indent-info))
            (comment (error "Comment indent should never happen"))
            (other (haskell-indent-other start end end-visible indent-info)))
    indent-info))

(defun haskell-indent-line-indentation (line-start line-end end-visible
                                         curr-line-type indent-info)
  "Separate a line of program into valdefs between offside keywords
and find indentation info for each part."
  (save-excursion
    (let (end-match beg-match in-string start-comment)
      ;; point is (already) at line-start
      (setq start-comment (haskell-indent-in-comment line-start line-end))
      (if start-comment                         ; if comment at the end
          (setq line-end (- start-comment 1)))  ; end line before it
      ;; loop on all parts separated by off-side-keywords
      (while (re-search-forward haskell-indent-off-side-keywords-re line-end t)
        (setq beg-match (match-beginning 0)); save beginning of match
        (setq end-match (match-end 0))  ; save end of match
        (if (haskell-indent-in-comment line-start (point)) ; keyword in a {- comment
            (progn
              (setq indent-info
                    (haskell-indent-valdef-indentation
                     line-start ; end line before comment
                     (- (search-backward "{-" line-start) 1)
                     end-visible curr-line-type indent-info))
                                        ;skip past end of comment
              (re-search-forward "-}[ \t]*" line-end 'move) 
              (setq line-start (point)))
          ;; not in a comment
          (setq in-string (haskell-indent-in-string line-start (point)))
          (if in-string
              (re-search-forward        ; skip past end of string
                                        ; line-start does not change...
               (concat (string (char-after in-string)) "[ \t]*") line-end 'move)
            ;; not in a string
            (if (< line-start beg-match) ; if off-side-keyword at the start
                (setq indent-info       ; do not try to find indentation points
                  (haskell-indent-valdef-indentation line-start beg-match
                                           end-visible
                                           curr-line-type indent-info)))
            ;; but  keep the start of the line if keyword alone on the line 
            (if (= line-end end-match)
                (haskell-indent-push-pos beg-match)))
            (setq line-start end-match)
            (goto-char line-start)))
      (setq indent-info
            (haskell-indent-valdef-indentation line-start line-end end-visible
                                     curr-line-type indent-info))))
  indent-info)


(defun haskell-indent-layout-indent-info (start contour-line)
  (let ((curr-line-type (haskell-indent-type-at-point))
	line-start line-end end-visible)
    (save-excursion
      (if (eq curr-line-type 'ident)
	  (let				; guess the type of line
	      ((sep
		(haskell-indent-separate-valdef
		 (point) (haskell-indent-get-end-of-line))))
	    ;; if the first ident is where or the start of a def
	    ;; keep it in a global variable
	    (if (string-match "where[ \t]*" (nth 1 sep))
		(setq haskell-indent-current-line-first-ident (nth 1 sep))
	      (if (nth 5 sep)		; is there a rhs-sign
		  (if (= (char-after (nth 5 sep)) ?\:) ;is it a typdef
		      (setq haskell-indent-current-line-first-ident "::")
		    (setq haskell-indent-current-line-first-ident
			  (nth 1 sep)))
		(setq haskell-indent-current-line-first-ident "")))))
      (while contour-line		; explore the contour points
	(setq line-start (pop contour-line))
	(goto-char line-start)
	(setq line-end (haskell-indent-get-end-of-line))
	(setq end-visible		; visible until the column of the
	      (if contour-line		; next contour point
		  (save-excursion
		    (move-to-column
		     (haskell-indent-point-to-col (car contour-line)))
		    (point))
		line-end))
	(if (and (not (haskell-indent-open-structure start line-start))
		 (not (haskell-indent-in-comment start line-start)))
	    (setq indent-info
		  (haskell-indent-line-indentation line-start line-end
						   end-visible curr-line-type
						   indent-info)))
	))))

(defun haskell-indent-find-let (start)
  (let ((open (haskell-indent-open-structure start (point))))
    (if open (setq start (1+ open))))
  (if (not (re-search-backward "\\<\\(in\\|let\\)\\>" start t))
      nil
    (let ((outer (or (haskell-indent-in-string start (point))
		     (haskell-indent-in-comment start (point))
		     (haskell-indent-open-structure start (point)))))
      (cond
       (outer
	(goto-char outer)
	(haskell-indent-find-let start))
       ((eq (char-after) ?i)
	;; Nested let.
	(and (haskell-indent-find-let start)
	     (haskell-indent-find-let start)))
       (t (point))))))

(defun haskell-indent-inside-comment (start)
  "Compute indent info for text inside comment.
START is the start position of the comment in which point is."
  ;; Ideally we'd want to guess whether it's commented out code or
  ;; whether it's text.  Instead, we'll assume it's text.
  (save-excursion
    (if (= start (point))
	;; We're actually not inside the comment.
	(cond
	 ((eq (char-after) ?\{) nil)	;Align as if it were code.
	 ((and (forward-comment -1)
	       (save-excursion (forward-line 2) (> (point) start)))
	  ;; We're after another comment and there's no empty line between us.
	  (haskell-indent-push-pos (point)))
	 (t nil))			;Else align as if it were code
     
      ;; We really are inside a comment.
      (haskell-indent-push-pos
       (if (looking-at "-}")
	   (progn
	     (forward-char 2)
	     (forward-comment -1)
	     (1+ (point)))
	 (let ((offset (if (looking-at "--?")
			   (- (match-beginning 0) (match-end 0)))))
	   (forward-line -1)		;Go to previous line.
	   (haskell-indent-back-to-indentation)
	   (if (< (point) start) (goto-char start))
	   (if (looking-at comment-start-skip)
	       (if offset (+ (point) 2 offset) (match-end 0))
	     (point))))))))

(defun haskell-indent-indentation-info ()
  "Return a list of possible indentations for the current line.
These are then used by `haskell-indent-cycle'."
  (let ((start (haskell-indent-start-of-def))
        (end (point))
        indent-info open follow contour-line)
    (cond
     ;; in string?
     ((setq open (haskell-indent-in-string start end))
      (haskell-indent-push-pos-offset open
				      (if (looking-at "\\\\") 0 1)))
      
     ;; in comment ?
     ((and (setq open (haskell-indent-in-comment start end))
	   (progn (haskell-indent-inside-comment open)
		  (if indent-info t	;We found a comment to align to.
		    ;; Else indent like following code.
		    (forward-comment (point-max))
		    (setq end (point))
		    nil))))

     ;; Closing the declaration part of a `let'.
     ((and (looking-at "in\\>")
	   (setq open (save-excursion (haskell-indent-find-let start)))
	   ;; For a "dangling let at EOL" we should use a different indentation
	   ;; scheme.
	   (save-excursion
	     (goto-char open)
	     (let ((letcol (current-column)))
	       (forward-word 1) (forward-comment 1)
	       (>= (current-column) letcol))))
      (haskell-indent-push-pos open))
     ;; open structure? ie  ( { [
     ((setq open (haskell-indent-open-structure start end))
      ;; there is an open structure to complete
      (if (looking-at "\\s)")
	  (haskell-indent-push-pos open) ; align a ) with (
	;; Anything else than a ) is subject to layout.
	(if (looking-at "\\s.\\|$ ")
	    (haskell-indent-push-pos open) ; align a punct with (
	  (setq follow (save-excursion
			 (goto-char (1+ open))
			 (haskell-indent-skip-blanks-and-newlines-forward end)
			 (point)))
	  (if (= follow end)
	      (haskell-indent-push-pos-offset open 1)
	    (haskell-indent-push-pos follow)))
	;; There might still be layout within the open structure.
	(let ((basic-indent-info (car indent-info))
	      (open-column (haskell-indent-point-to-col open))
	      (contour-line (haskell-indent-contour-line (1+ open) end)))
	  (assert (null (cdr indent-info)))
	  (when contour-line
	    (setq indent-info nil)
	    (haskell-indent-layout-indent-info (1+ open) contour-line)
	    ;; Fix up indent info.
	    (let ((base-elem (assoc open-column indent-info)))
	      (if base-elem
		  (progn (setcar base-elem (car basic-indent-info))
			 (setcdr base-elem (cdr basic-indent-info)))
		(setq indent-info
		      (append indent-info (list basic-indent-info)))))))))
     ;; full indentation
     ((setq contour-line (haskell-indent-contour-line start end))
      (haskell-indent-layout-indent-info start contour-line))
     (t
      ;; simple contour just one indentation at start
      (if (and (eq haskell-literate 'bird)
	       (eq (haskell-indent-point-to-col start) 1))
	  ;; for a Bird style literate script put default offset
	  ;; in the case of no indentation
	  (haskell-indent-push-col
	   (1+ haskell-indent-literate-Bird-default-offset))
	(haskell-indent-push-pos start))))
    indent-info))

(defun haskell-indent-event-type (event)
  "Check if EVENT is the TAB or RET key before returning the value
of `event-basic-type'.  Needed for dealing with the case that Emacs
is not in a windowing environment."
  (cond ((eq event ?\t) 'tab)
        ((eq event ?\r) 'return)
        (t (event-basic-type event)))
  )
  
(defun haskell-indent-cycle ()
  "Indentation cycle.
We stay in the cycle as long as the TAB key is pressed.
Any other key or mouse click terminates the cycle and is interpreted
with the exception of the RET key which merely exits the cycle."
  (interactive "*")
  (if (and haskell-literate
           (not (haskell-indent-within-literate-code)))
      (indent-to-left-margin)  ;; use the ordinary tab for text...
    (let (il indent-list com indent-info cdrii marker
             (last-insert-length 0))
      (if (> (current-column) (haskell-indent-current-indentation))
          (setq marker (point-marker)))
      (haskell-indent-back-to-indentation)
      (save-excursion
	(setq il (setq indent-list (haskell-indent-indentation-info))))
      ;;(message "Indent-list:%s" indent-list) (read-event) ; uncomment for debug!!
      (setq indent-info (car il))
      (haskell-indent-line-to (car indent-info)) ; insert indentation
      (if (setq cdrii (cdr indent-info))
          (progn
            (insert cdrii)
            (setq last-insert-length (length cdrii)))
        (setq last-insert-length 0))
      (if (= (length indent-list) 1)
          (message "Sole indentation")
        (message (format "Indent cycle (%d)..." (length indent-list)))
        (while (equal (haskell-indent-event-type (setq com (read-event))) 'tab)
          (setq il (cdr il))            ; get next insertion
          (or il (setq il indent-list)) ; if at the end of insertion, restart
                                        ; from the beginning
          (setq indent-info (car il))
          (haskell-indent-line-to (car indent-info)) ; insert indentation
          (delete-char last-insert-length)
          (if (setq cdrii (cdr indent-info))
              (progn
                (insert cdrii)
                (setq last-insert-length (length cdrii)))
            (setq last-insert-length 0))
          (message "Indenting..."))
        (if (not (equal (haskell-indent-event-type com) 'return))
            (setq unread-command-events (list com)))
        (message "Done."))
      (if marker
          (goto-char (marker-position marker)))
      ))
  )
;;; alignment functions
;;;
(defun haskell-indent-shift-columns (dest-column region-stack)
  "Shifts columns in region-stack to go to DEST-COLUMN.
Elements of the stack are pairs of points giving the start and end
of the regions to move."
  (let (reg col diffcol reg-end)
    (while (setq reg (pop region-stack))
      (setq reg-end (copy-marker (cdr reg)))
      (goto-char (car reg))
      (setq col (current-column))
      (setq diffcol (- dest-column col))
      (if (not (zerop diffcol))
          (catch 'end-of-buffer
            (while (<= (point) (marker-position reg-end))
              (if (< diffcol 0)
                  (backward-delete-char-untabify (- diffcol) nil)
                (insert-char ?\  diffcol))
              (end-of-line 2)           ; should be (forward-line 1)
              (if (eobp)                ; but it adds line at the end...
                  (throw 'end-of-buffer nil))
              (move-to-column col)))
        ))
    ))
                
(defun haskell-indent-align-def (p-arg type)
  "Align guards or rhs within the current definition before point.
If P-ARG is t align all defs up to the mark.
TYPE is either 'guard or 'rhs."
  (save-excursion
    (let (start-block end-block
          (maxcol (if (eq type 'rhs) haskell-indent-rhs-align-column 0)) 
          contour sep defname defnamepos
          defcol pos lastpos
          regstack eqns-start start-found)
      ;; find the starting and ending boundary points for alignment 
      (if p-arg                         
          (if (mark)                    ; aligning everything in the region
            (progn
              (when (> (mark) (point)) (exchange-point-and-mark))
              (setq start-block
                    (save-excursion
                      (goto-char (mark))
                      (haskell-indent-get-beg-of-line)))
              (setq end-block
                  (progn (if (haskell-indent-bolp)
                             (haskell-indent-forward-line -1))
                         (haskell-indent-get-end-of-line))))
            (error "The mark is not set for aligning definitions"))
        ;; aligning the current definition
        (setq start-block (haskell-indent-start-of-def))
        (setq end-block (haskell-indent-get-end-of-line)))
      ;; find the start of the current valdef using the contour line
      ;; in reverse order because we need the nearest one from the end
      (setq contour
            (reverse (haskell-indent-contour-line start-block end-block)))
      (setq pos (car contour))          ; keep the start of the first contour
      ;; find the nearest start of a definition
      (while (and (not defname) contour)
        (goto-char (pop contour))
        (if (haskell-indent-open-structure start-block (point))
            nil
          (setq sep (haskell-indent-separate-valdef (point) end-block))
          (if (nth 5 sep)               ; is there a rhs?
              (progn (setq defnamepos (nth 0 sep))
                     (setq defname (nth 1 sep))))))
      ;; start building the region stack
      (if defnamepos
          (progn                        ; there is a valdef
            ;; find the start of each equation or guard
            (if p-arg      ; when indenting a region
                ;; accept any start of id or pattern as def name
                (setq defname "\\<\\|("))
            (setq defcol (haskell-indent-point-to-col defnamepos))
            (goto-char pos)
            (setq end-block (haskell-indent-get-end-of-line))
            (catch 'top-of-buffer
              (while (and (not start-found)
                          (>= (point) start-block))
                (if (<= (haskell-indent-current-indentation) defcol)
                    (progn
                      (move-to-column defcol)
                      (if (and (looking-at defname) ; start of equation
                               (not (haskell-indent-open-structure start-block (point))))
                          (push (cons (point) 'eqn) eqns-start)
                        ;; found a less indented point not starting an equation
                        (setq start-found t)))
                  ;; more indented line
                  (haskell-indent-back-to-indentation)
                  (if (and (eq (haskell-indent-type-at-point) 'guard) ; start of a guard
                           (not (haskell-indent-open-structure start-block (point))))
                      (push (cons (point) 'gd) eqns-start)))
                (if (bobp)          
                    (throw 'top-of-buffer nil)
                  (haskell-indent-backward-to-indentation 1))))
            ;; remove the spurious guards before the first equation
            (while (and eqns-start (eq (cdar eqns-start) 'gd))
              (pop eqns-start))
            ;; go through each equation to find the region to indent
            (while eqns-start
              (let ((eqn (caar eqns-start)))
		(setq lastpos (if (cdr eqns-start)
				  (save-excursion
				    (goto-char (caadr eqns-start))
				    (haskell-indent-forward-line -1)
				    (haskell-indent-get-end-of-line))
				end-block))
		(setq sep (haskell-indent-separate-valdef eqn lastpos)))
              (if (eq type 'guard)
                  (setq pos (nth 3 sep))
                ;; check if what follows a rhs sign is more indented or not
                (let ((rhs (nth 5 sep))
                      (aft-rhs (nth 6 sep)))
                  (if (and rhs aft-rhs
                           (> (haskell-indent-point-to-col rhs)
                              (haskell-indent-point-to-col aft-rhs)))
                      (setq pos aft-rhs)
                    (setq pos rhs))))
              (if pos
                  (progn                ; update region stack
                    (push (cons pos (or lastpos pos)) regstack)
                    (setq maxcol        ; find the highest column number
                          (max maxcol
                               (progn   ;find the previous non-empty column
                                 (goto-char pos)
                                 (skip-chars-backward
                                  " \t"
                                  (haskell-indent-get-beg-of-line))
                                 (if (haskell-indent-bolp)
                                     ;;if on an empty prefix
                                     (haskell-indent-point-to-col pos) ;keep original indent
                                   (1+ (haskell-indent-point-to-col (point)))))))))
              (pop eqns-start))
            ;; now shift according to the region stack
            (if regstack
                (haskell-indent-shift-columns maxcol regstack))
            ))
      )))

(defun haskell-indent-align-guards-and-rhs (start end)
  "Align the guards and rhs of functions in the region which must be active."
  (interactive "*r")
  (haskell-indent-align-def t 'guard)
  (haskell-indent-align-def t 'rhs))

;;;  insertion functions
;;;
(defun haskell-indent-insert-equal ()
  "Insert an = sign and align the previous rhs of the current function."
  (interactive "*")
  (if (or (haskell-indent-bolp)
          (/= (preceding-char) ?\ ))
      (insert ?\ ))
  (insert "= ")
  (haskell-indent-align-def (haskell-indent-mark-active) 'rhs))

(defun haskell-indent-insert-guard (&optional text)
  "Insert a guard sign (|) followed by optional TEXT and align the
previous guards of the current function.  
Alignment works only if all guards are to the south-east of their |."
  (interactive "*")
  (let ((pc (if (haskell-indent-bolp) ?\012
                (preceding-char)))
        (pc1 (or (char-after (- (point) 2)) 0)))
    ;; check what guard to insert depending on the previous context
    (if (= pc ?\ )                      ; x = any char other than blank or |
        (if (/= pc1 ?\|)
            (insert "| ")               ; after " x" 
          ())                           ; after " |"
      (if (= pc ?\|)
          (if (= pc1 ?\|)
              (insert " | ")            ; after "||"
            (insert " "))               ; after "x|"
        (insert " | ")))                ; general case
    (if text (insert text))
    (haskell-indent-align-def (haskell-indent-mark-active) 'guard)))

(defun haskell-indent-insert-otherwise ()
  "Insert a guard sign (|) followed by 'otherwise' and align the
previous guards of the current function."
  (interactive "*")
  (haskell-indent-insert-guard "otherwise")
  (haskell-indent-insert-equal))

(defun haskell-indent-insert-where ()
  "Insert and a where keyword at point and indent the resulting
line with an indentation cycle."
  (interactive "*")
  (insert "where ")
  (save-excursion
    (haskell-indent-cycle)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; turn-on-haskell-indent to be used in conjunction with
;;; the haskell-mode of Graeme E Moss and Tommy Thorn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun turn-on-haskell-indent ()
  "Turn on ``intelligent'' haskell indentation mode."
  (interactive)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'haskell-indent-cycle)
  ;; Removed: remapping DEL seems a bit naughty --SDM
  ;; (local-set-key "\177"  'backward-delete-char-untabify)
  ;; The binding to TAB is already handled by indent-line-function.  --Stef
  ;; (local-set-key "\t"    'haskell-indent-cycle)
  (local-set-key [?\C-c ?\C-=] 'haskell-indent-insert-equal)
  (local-set-key [?\C-c ?\C-|] 'haskell-indent-insert-guard)
  (local-set-key [?\C-c ?\C-o] 'haskell-indent-insert-otherwise)
  (local-set-key [?\C-c ?\C-w] 'haskell-indent-insert-where)
  (local-set-key [?\C-c ?\C-.] 'haskell-indent-align-guards-and-rhs)
  (local-set-key [?\C-c ?\C->] 'haskell-indent-put-region-in-literate)
  (setq haskell-indent-mode t)
  (run-hooks 'haskell-indent-hook)
  )

(defun turn-off-haskell-indent ()
  "Turn off ``intelligent'' haskell indentation mode that deals with
the layout rule of Haskell."
  (interactive)
  (kill-local-variable 'indent-line-function)
  ;; (local-unset-key "\t")
  ;; (local-unset-key "\177")
  (local-unset-key [?\C-c ?\C-=])
  (local-unset-key [?\C-c ?\C-|])
  (local-unset-key [?\C-c ?\C-o])
  (local-unset-key [?\C-c ?\C-w])
  (local-unset-key [?\C-c ?\C-.])
  (local-unset-key [?\C-c ?\C->])
  (setq haskell-indent-mode nil)
  )

(defvar haskell-indent-mode nil
  "Indicates if the semi-intelligent Haskell indentation mode is in effect
in the current buffer.")
(make-variable-buffer-local 'haskell-indent-mode)

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'haskell-indent-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((haskell-indent-mode " Ind")))))

(defun haskell-indent-mode (&optional arg)
  "``intelligent'' Haskell indentation mode that deals with
the layout rule of Haskell.  \\[haskell-indent-cycle] starts the cycle
which proposes new possibilities as long as the TAB key is pressed. 
Any other key or mouse click terminates the cycle and is interpreted
except for RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Note: \\[indent-region] which applies \\[haskell-indent-cycle] for each line
of the region also works but it stops and asks for any line having more
than one possible indentation.
Use TAB to cycle until the right indentation is found and then RET to go the
next line to indent.

Invokes `haskell-indent-hook' if not nil."
  (interactive "P")
  (setq haskell-indent-mode
        (if (null arg) (not haskell-indent-mode)
          (> (prefix-numeric-value arg) 0)))
  (if haskell-indent-mode
      (turn-on-haskell-indent)
    (turn-off-haskell-indent)))

(provide 'haskell-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Haskell-stand-alone- indentation mode (vanilla version of the Hugs-mode)
;;; in the case where only Haskell indentation is used without the hugs-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hugs-syntax-table
  (let ((table (copy-syntax-table)))
    (modify-syntax-entry ?_   "w " table)   
    (modify-syntax-entry ?`   "w " table)
    (modify-syntax-entry ?\'  "w " table)
    (modify-syntax-entry ?\(  "()" table)
    (modify-syntax-entry ?\)  ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)
    (modify-syntax-entry ?\"  "\"" table)
    (modify-syntax-entry ?\\  "\\" table)
	  
    (if haskell-running-xemacs
	;; XEmacs specific syntax-table.
	(progn
	  (modify-syntax-entry ?-   ". 2356" table)  ;  --  starts a comment.
	  (modify-syntax-entry ?\n  "> b   " table)  ;  \n  ends a comment.
	  (modify-syntax-entry ?{   ". 1   " table)  ;  {-  starts a nested comment.
	  (modify-syntax-entry ?}   ". 4   " table)  ;  -}  ends a nested comment.
	  )
      ;; Emacs specific syntax-table.
      ;; Actually the `b' is ignored, so it only works correctly in Emacs-21
      ;; where the `n' is understood.
      (modify-syntax-entry ?{  "(}1nb" table)
      (modify-syntax-entry ?}  "){4nb" table)
      (modify-syntax-entry ?-  "_ 123" table)
      (modify-syntax-entry ?\n ">" table)
      )
    table)
  "Syntax table in use in `hugs-mode'.")

(defvar hugs-mode-map (make-sparse-keymap)
  "Keymap used in `hugs-mode'.")

(defun haskell-stand-alone-indent-mode ()
  "Major mode for indenting Haskell source files.

COMMANDS
\\{hugs-mode-map}\

TAB indents for Haskell code.  Delete converts tabs to spaces as it moves back.

Variables controlling indentation/edit style:

 `haskell-indent-offset'      (default 4)
    Indentation of Haskell statements with respect to containing block.

See also the user variables `hugs-type-keywords' and `hugs-start-keywords'

Entry to this mode calls the value of `hugs-mode-hook' if that value
is non-nil."
  (interactive)
  ;; Set up local variables.
  (kill-all-local-variables)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'indent-line-function)

  (set-syntax-table hugs-syntax-table)
  (setq major-mode             'haskell-indent-mode
	mode-name              "Haskell-Indent"
	comment-start          "{-"
	comment-start-skip     "{-[^a-zA-Z0-9]*"
	comment-end            "-}"
	indent-line-function   'haskell-indent-line)
  (use-local-map hugs-mode-map)

  (run-hooks 'hugs-mode-hook)
)

(provide 'haskell-indent)
;;; haskell-indent.el ends here
