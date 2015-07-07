

(eval-when-compile
  (require 'rx))


(defconst haskell-lexeme-varid
  "[[:lower:]_][[:alnum:]'_]*")

(defconst haskell-lexeme-conid
  "[[:upper:]][[:alnum:]'_]*")

(defconst haskell-lexeme-varid-or-conid
  "[[:alpha:]_][[:alnum:]'_]*")

(defconst haskell-lexeme-reservedid
  (rx (| "case"
         "class"
         "data"
         "default"
         "deriving"
         "do"
         "else"
         "foreign"
         "if"
         "import"
         "in"
         "infix"
         "infixl"
         "infixr"
         "instance"
         "let"
         "module"
         "newtype"
         "of"
         "then"
         "type"
         "where"
         "_")))


(defconst haskell-lexeme-varsym
  "[!#$%&*+./<=>?@^|~\\-][!#$%&*+./:<=>?@^|~\\-]*")

(defconst haskell-lexeme-consym
  ":[!#$%&*+./:<=>?@^|~\\-]*")

(defconst haskell-lexeme-varsym-or-consym
  "[!#$%&*+./:<=>?@^|~\\-]+")

(defconst haskell-lexeme-modid-opt-prefix
  (concat "\\(?:" haskell-lexeme-conid "\\.\\)*"))

(defconst haskell-lexeme-qvarid
  (concat haskell-lexeme-modid-opt-prefix haskell-lexeme-varid))

(defconst haskell-lexeme-qconid
  (concat haskell-lexeme-modid-opt-prefix haskell-lexeme-conid))

(defconst haskell-lexeme-qvarid-or-qconid
  (concat haskell-lexeme-modid-opt-prefix haskell-lexeme-qvarid-or-qconid))


(defconst haskell-lexeme-qvarsym
  (concat haskell-lexeme-modid-opt-prefix haskell-lexeme-varsym))

(defconst haskell-lexeme-qconsym
  (concat haskell-lexeme-modid-opt-prefix haskell-lexeme-consym))

(defconst haskell-lexeme-qvarsym-or-qconsym
  (concat haskell-lexeme-modid-opt-prefix haskell-lexeme-varsym-or-consym))

(defconst haskell-lexeme-decimal
  "[[:digit:]]+")

(defconst haskell-lexeme-octal
  "0[oO][0-7]+")

(defconst haskell-lexeme-hexadecimal
  "0[xX][[:xdigit:]]+")

(defconst haskell-lexeme-float
  (rx (| (regexp "[0-9]+\\." (opt (regexp "[eE][-+][0-9]+")))
         (regexp "[0-9]+[eE][-+][0-9]+"))))

(defconst haskell-lexeme-char-literal-inside
  (rx (| (regexp "[^\n'\\]")
         (: "\\"
            (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'"
               "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
               "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
               "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
               "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
               (regexp "^[A-Z@\\[\\]^_\\\\]")

        )))))

(defconst haskell-lexeme-char-literal
  (concat "'" haskell-lexeme-char-literal-inside "'"))

(defconst haskell-lexeme-string-literal-inside
  (rx (* (| (regexp "[^\n\"\\]")
            (: "\\"
               (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'" "&"
                  "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
                  "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
                  "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
                  "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
                  (regexp "^[A-Z@\\[\\]^_\\\\]")
                  (regexp "\\\\[[:space:]]*\\\\")

                  ))))))

(defconst haskell-lexeme-string-literal
  (concat "\"" haskell-lexeme-string-literal-inside "\""))
