(require 'ert)
(require 'haskell-font-lock)
(require 'haskell-mode)

(defun insert-lines (&rest lines)
  (dolist (line lines)
    (insert line)
    (insert "\n")))


(defun check-syntax-and-face-match-range (beg end syntax face)
  "Check if all charaters between positions BEG and END have
syntax set to SYNTAX and face set to FACE.

If SYNTAX or FACE are set to t then any syntex respective face is
not checked."
  (let (all-syntaxes
        all-faces
        (text (buffer-substring-no-properties beg end)))
    (while (< beg end)
      (add-to-list 'all-syntaxes (syntax-class (syntax-after beg)))
      (add-to-list 'all-faces (get-text-property beg 'face))
      (setq beg (1+ beg)))
    (unless (eq syntax t)
      (should (equal (list text (list (syntax-class (string-to-syntax syntax))))
                     (list text all-syntaxes))))
    (unless (eq face t)
      (should (equal (list text (list face))
                     (list text all-faces))))))

(defun check-face-match-range (face 0)
  (let ((beg (match-beginning 0))
        (end (match-end 0)))
    (while (< beg end)
      (should (eq face (get-text-property beg 'face)))
      (setq beg (1+ beg)))))

(defmacro with-haskell-mode-buffer (&rest body)
  "Run BODY in the context of a new buffer set to `haskell-mode'.

Buffer is named *haskell-mode-buffer*. It is not deleted
after a test as this aids interactive debugging."
  (declare (indent 1) (debug t))
  `(progn
     ;; we want to create buffer from scratch so that there are no
     ;; leftover state from the previous test
     (when (get-buffer "*haskell-mode-buffer*")
       (kill-buffer "*haskell-mode-buffer*"))
     (save-current-buffer
       (set-buffer (get-buffer-create "*haskell-mode-buffer*"))
       (haskell-mode)
       ,@body)))

(defun check-properties (lines props)
  "Check if syntax properties and font-lock properties as set properly.

LINES is a list of strings that will be inserted to a new
buffer. Then PROPS is a list of tripples of (string syntax
face). String is searched for in the buffer and then is checked
if all of its characters have syntax and face. See
`check-syntax-and-face-match-range`."
  (when (get-buffer "*haskell-mode-buffer*")
    (kill-buffer "*haskell-mode-buffer*"))
  (save-current-buffer
    (set-buffer (get-buffer-create "*haskell-mode-buffer*"))
    (haskell-mode)
    (dolist (line lines)
      (insert line)
      (insert "\n"))
    (font-lock-fontify-buffer)
    (goto-char (point-min))
    (dolist (prop props)
      (cl-destructuring-bind (string syntax face) prop
        (search-forward string)
        (check-syntax-and-face-match-range (match-beginning 0) (match-end 0) syntax face)))))

(ert-deftest haskell-syntactic-test-1 ()
  "Simple keywords fontified"
  (check-properties
    '("module Test where")
    '(("module" "w" haskell-keyword-face)
      ("Test" "w" haskell-constructor-face)
      ("where" "w" haskell-keyword-face))))

(ert-deftest haskell-syntactic-test-4 ()
  "Apostrophe as part of a contructor token."
  :expected-result :failed
  (check-properties
   '("T_'ttt_'t_' T Tx T'x T_x T_'_")
   '(("T_'ttt_'t_'" "w" haskell-constructor-face)
     ("T" "w" haskell-constructor-face)
     ("T'x" "w" haskell-constructor-face)
     ("T_x" "w" haskell-constructor-face)
     ("T_'_" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-5 ()
  "Apostrophe inside a token."
  :expected-result :failed
  (check-properties
   '("_T_'tt't_'t_' xxx'ff _f _'''")
   '(("_T_'tt't_'t_'" "w" haskell-symbol-face)
     ("xxx'ff" "w" haskell-symbol-face)
     ("_f" "w" haskell-symbol-face)
     ("_'''" "w" haskell-symbol-face))))


(ert-deftest haskell-syntactic-test-7 ()
  "Take quotes and double quotes under control."
  (check-properties
   '("\"\'\" Cons1"
     "\'\"\' Cons2")
   '(("\"\'\"" t font-lock-string-face)
     ("Cons1" "w" haskell-constructor-face)
     ("\'\"\'" t font-lock-string-face)
     ("Cons2" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-7b ()
  "Take quotes and double quotes under control."
  :expected-result :failed
  (check-properties
    ;; do not get fooled
   '("\"\'\"\'\"\'\"\'\"\'\"\'\"\'\"\'\"\' Cons")
   '(("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-7c ()
  "Tripple backslash in a string that also has a quote."
  (check-properties
   ;; the below is: "\"\\\"" and \\\ get marked as punctuation because
   ;; of detecting -- that is a part of larger non-comment lexeme
   '("  \"\\\"\\\\\\\"\" Cons")
   '(("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-8 ()
  "Check if gap-rule works."
  (check-properties
   '("\"\\  \\\\\\ \\  "
     "   \\\" Cons")
   '(("\\" "\\" t)               ; 1st is escape
     ("\\"  "." t)               ; 2nd is punctuation
     ("\\" "\\" t)               ; 3rd is escape
     ("\\"  "." t)               ; 4th is punctuation
     ("\\" "\\" t)               ; 5th is escape
     ("\\"  "." t)               ; 6th is punctuation
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-9 ()
  "Syntax for hierarchical modules."
  ; note that quite many things here are not consistent but for now
  ; this test describes what it is
  (check-properties
   '(" A.B.C"
     " D.E.f"
     " G.H.>>="
     " <=<"
     )
   '(("A" "w" haskell-constructor-face)
     ("." "." haskell-constructor-face)
     ("B" "w" haskell-constructor-face)
     ("." "." haskell-constructor-face)
     ("C" "w" haskell-constructor-face)

     ("D" "w" haskell-constructor-face)
     ("." "." haskell-constructor-face)
     ("E" "w" haskell-constructor-face)
     ("." "." haskell-operator-face)
     ("f" "w" nil)

     ("G" "w" haskell-constructor-face)
     ("." "." haskell-constructor-face)
     ("H" "w" haskell-constructor-face)
     ("." "." haskell-operator-face)        ; this is wrong
     (">>=" "." haskell-operator-face)

     ("<=<" "." haskell-operator-face))))

(ert-deftest haskell-syntactic-test-9a ()
  "Syntax for hierarchical modules when on the first line."
  ;; note that quite many things here are not consistent but for now
  ;; this test describes what it is. When on the first column
  ;; font-lock thins we are defining (.) operator. Not good.
  :expected-result :failed
  (check-properties
   '("A1.B.C"
     "A2.B.c"
     "A3.B.>>="
     "<=<"
     )
   '(("A1" "w" haskell-constructor-face)
     ("."  "." haskell-constructor-face)
     ("B"  "w" haskell-constructor-face)
     ("."  "." haskell-constructor-face)
     ("C"  "w" haskell-constructor-face)

     ("A2" "w" nil)
     ("."  "." nil)
     ("B"  "w" nil)
     ("."  "." nil)
     ("C"  "w" nil)

     ("A3" "w" haskell-constructor-face)
     ("."  "." haskell-constructor-face)
     ("B"  "w" haskell-constructor-face)
     ("."  "." haskell-operator-face)        ; this is wrong
     (">>="  "." haskell-operator-face)

     ("<=<"  "." haskell-operator-face))))


(ert-deftest haskell-syntactic-test-10 ()
  "Syntax for comments"
  (check-properties
   '(" Cons0 -- Comm1"
     " --\ Cons2"
     " ----- Comment3"
     " {- Comm4 -} -- Comm5"
     " -- \" Comm6"
     " Cons7"
     "{-# pragma1 #-}"
     "{-# non_pragma2 -}"
     "{- non_pragma3 #-}"
     )
   '(("Cons0" "w" haskell-constructor-face)
     ("Comm1" "w" font-lock-comment-face)
     ;("Cons2" "w" haskell-constructor-face) -- works in real life, does not work in tests...
     ("Comment3" "w" font-lock-comment-face)
     ("Comm4"  "w" font-lock-comment-face)

     ("Comm5" "w" font-lock-comment-face)
     ("Comm6" "w" font-lock-comment-face)
     ("Cons7"  "w" haskell-constructor-face)
     ("pragma1"  "w" haskell-pragma-face)
     ("non_pragma2"  "w" font-lock-comment-face)

     ("non_pragma3" "w" font-lock-comment-face))))


(ert-deftest haskell-syntactic-string-vs-comment-escape ()
  "Check string escape vs comment escape"
  :expected-result :failed
  (check-properties
   ;; "\"" \--  Cons
   '("\"\\\"\" \\--  Cons")
   '(("\\--" "." haskell-operator-face)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-quasiquote-no-escape ()
  "Check string escape vs comment escape"
  (check-properties
   '("[qq| \\|]  Cons")
   '(("qq" "w" nil)
     ("\\" "." font-lock-string-face)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-quasiquote-three-punctuation ()
  "Check string escape vs comment escape"
  (check-properties
   '("[qq| %\\|]  Cons")
   '(("qq" "w" nil)
     ("%\\" "." font-lock-string-face)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-11 ()
  "Syntax for haddock comments"
  (check-properties
   '(" -- | Dcom1"                      ; haddocks
     " -- $ Dcom2"
     " -- ^ Dcom3"
     " -- * Dcom4"
     " --| Cons5"                       ; non-haddocks, operators
     " --$ Cons6"
     " --^ Cons7"
     " --* Cons8"
     " --  | Com5"                      ; non-haddocks, comments
     " --  $ Com6"
     " --  ^ Com7"
     " --  * Com8"
     " {-| Dcom10 -}"                   ; haddocks
     " {-$ dcom11 -}"
     " {-^ Dcom12 -}"
     " {-* Dcom13 -}"
     " {- | Dcom14 -}"                  ; also haddocks
     " {- $ Dcom15 -}"
     " {- ^ Dcom16 -}"
     " {- * Dcom17 -}")
   '(("Dcom1" "w" font-lock-doc-face)
     ("Dcom2" "w" font-lock-doc-face)
     ("Dcom3" "w" font-lock-doc-face)
     ("Dcom4" "w" font-lock-doc-face)
     ("Cons5" "w" haskell-constructor-face)
     ("Cons6" "w" haskell-constructor-face)
     ("Cons7" "w" haskell-constructor-face)
     ("Cons8" "w" haskell-constructor-face)
     ("Com5" "w" font-lock-comment-face)
     ("Com6" "w" font-lock-comment-face)
     ("Com7" "w" font-lock-comment-face)
     ("Com8" "w" font-lock-comment-face)
     ("Dcom10" "w" font-lock-doc-face)
     ("Dcom11" "w" font-lock-doc-face)
     ("Dcom12" "w" font-lock-doc-face)
     ("Dcom13" "w" font-lock-doc-face)
     ("Dcom14" "w" font-lock-doc-face)
     ("Dcom15" "w" font-lock-doc-face)
     ("Dcom16" "w" font-lock-doc-face)
     ("Dcom17" "w" font-lock-doc-face)
     )))

(ert-deftest haskell-syntactic-test-11 ()
  "Syntax for haddock comments"
  ;; Note: all of these are prefixed with space so that
  ;; top-level definition detection does not kick in.
  :expected-result :failed
  (check-properties
   '(" 'a''b'"                          ; ('a','b')
     " 12'c'"                           ; (12,'c')
     " 0x32'd'"                         ; (0x34,'d')
     " e56'f'"                          ; Not in scope: ‘e56'f'’
     " 'g''h''" ; lexical error in string/character literal at end of input
     " 'i'j "                           ; ('i',45)
     " 'k''l"                           ; ('k',l_1627393257)
     " \"m\"'n'"                        ; ("m",'n')
     " 'o'\"p\""                        ; ('o',"p")
     " '\"'\"'\""                       ; ('"',"'")
     " 7e-8'q'"                         ; (7e-8,'q')
     " 9.9'r'"                          ; (9.9,'r')
     " 's'12e-34"                       ; ('s',1.2e-33)
     " 't'56.78"                        ; ('t',56.78)
     )
   '(("'a'" t font-lock-string-face)
     ("'b'" t font-lock-string-face)
     ("12" t nil)
     ("'c'" t font-lock-string-face)
     ("0x32" t nil)
     ("'d'" t font-lock-string-face)
     ("e56'f'" t nil)
     ("'g'" t font-lock-string-face)
     ("'h'" t font-lock-string-face)
     ("'" t nil)  ; ?? stray apostrophe is what??
     ("'i'" t font-lock-string-face)
     ("j" t nil)
     ("'k'" t font-lock-string-face)
     ("'l" t nil) ;; apostrophe here should be prefix operator
     ("\"m\"'n'" t font-lock-string-face)
     ("'o'\"p\"" t font-lock-string-face)
     ("'\"'\"'\"" t font-lock-string-face)
     ("7e-8" t nil)
     ("'q'" t font-lock-string-face)
     ("9.9" t nil)
     ("'r'" t font-lock-string-face)
     ("'s'" t font-lock-string-face)
     ("12e-34" t nil)
     ("'t'" t font-lock-string-face)
     ("56.78" t nil)
     )))


(ert-deftest haskell-syntactic-test-quasiquoter-1 ()
  "Basic syntax for QuasiQuote"
  (check-properties
   '("v = [quoter| string |] Cons")
   '(("[" t nil)
     ("|" t font-lock-string-face)
     ("string" t font-lock-string-face)
     ("|" t font-lock-string-face)
     ("]" t nil)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-quasiquoter-2 ()
  "Basic syntax for QuasiQuote multiline"
  (check-properties
   '("v = [quoter| string"
     " one more  | ]"
     " finishing line"
     "|] Cons")
   '(("[" t nil)
     ("|" t font-lock-string-face)
     ("string" t font-lock-string-face)
     ("line" t font-lock-string-face)
     ("|" t font-lock-string-face)
     ("]" t nil)
     ("Cons" "w" haskell-constructor-face))))

(ert-deftest haskell-syntactic-test-quasiquoter-2 ()
  "QuasiQuote inside quasi quote"
  (check-properties
   '("v = [quoter| [inner| string {- -- |] Outside |]")
   '(("[" t nil)
     ("|" t font-lock-string-face)
     ("inner" t font-lock-string-face)
     ("string" t font-lock-string-face)
     ("|" t font-lock-string-face)
     ("]" t nil)
     ("Outside" "w" haskell-constructor-face)
     )))

(ert-deftest haskell-syntactic-test-quasiquoter-3 ()
  "QuasiQuote inside comment"
  (check-properties
   '("v = -- [quoter| "
     "    [inner| string {- -- |] Outside1 |] Outside2")
   '(("quoter" t font-lock-comment-face)
     ("inner" t nil)
     ("string" t font-lock-string-face)
     ("|" t font-lock-string-face)
     ("]" t nil)
     ("Outside1" "w" haskell-constructor-face)
     ("Outside2" "w" haskell-constructor-face)
     )))

(ert-deftest haskell-syntactic-test-quasiquoter-3 ()
  "QuasiQuote should not conflict with TemplateHaskell"
  (check-properties
   '("nope = [| Cons |]"
     "nope = [e| Cons_e |]"
     "nope = [t| Cons_t |]"
     "nope = [d| Cons_d |]"
     "nope = [p| Cons_p |]"
     "yes = [x| Cons_x |]")
   '(("Cons" t haskell-constructor-face)
     ("Cons_e" t haskell-constructor-face)
     ("Cons_t" t haskell-constructor-face)
     ("Cons_d" t haskell-constructor-face)
     ("Cons_p" t haskell-constructor-face)
     ("Cons_x" t font-lock-string-face))))

(ert-deftest haskell-syntactic-test-special-not-redefined ()
  "QuasiQuote should not conflict with TemplateHaskell"
  (check-properties
   '("nope1, nope2 "
     "nope3 = nope4 "
     "nope -> nope"
     "nope :: nope"
     "nope <- nope"
     "nope ` nope")
   '(("," t haskell-operator-face)
     ("=" t haskell-operator-face)
     ("->" t haskell-operator-face)
     ("::" t haskell-operator-face)
     ("<-" t haskell-operator-face)
     ("`" t nil))))

(ert-deftest haskell-syntactic-definition-face-1 ()
  (check-properties
   '("F +++ G")
   '(("+++" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-1a ()
  (check-properties
   '("F `abc` G")
   '(("abc" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-2 ()
  :expected-result :failed
  (check-properties
   '("M.F +++ N.G")
   '(("+++" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-2a ()
  :expected-result :failed
  (check-properties
   '("M.F `abc` N.G")
   '(("abc" t haskell-definition-face))))

(ert-deftest haskell-syntactic-definition-face-3 ()
  (check-properties
   '("Q +++ 12.12")
   '(("+++" t haskell-definition-face))))
