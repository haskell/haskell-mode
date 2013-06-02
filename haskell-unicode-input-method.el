;; Haskell Unicode helper functions
;;
;; Copyright 2010—2011 Roel van Dijk
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;;     * The name of Roel van Dijk and the names of contributors may
;;       NOT be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(provide 'haskell-unicode-input-method)
(require 'quail)

(quail-define-package
 "haskell-unicode" ;; name
 "UTF-8"           ;; language
 "\\"              ;; title
 t                 ;; guidance
 "Haskell Unicode input method.
Designed to be used with the Haskell UnicodeSyntax language
extension in combination with the x-unicode-symbols set of
packages (base-unicode-symbols and containers-unicode-symbols).
"                  ;; docstring
  nil              ;; translation-keys
  nil              ;; forget-last-selection
  nil              ;; deterministic
  nil              ;; kbd-translate
  nil              ;; show-layout
  nil              ;; create-decode-map
  nil              ;; maximum-shortest
  nil              ;; overlay-plist
  nil              ;; update-translation-function
  nil              ;; conversion-keys
  t                ;; simple
  )

(quail-define-rules
 ;; Greek letters
 ("alpha "           ["α"])
 ("Alpha "           ["Α"])
 ("beta "            ["β"])
 ("Beta "            ["Β"])
 ("gamma "           ["γ"])
 ("Gamma "           ["Γ"])
 ("delta "           ["δ"])
 ("Delta "           ["Δ"])
 ("epsilon "         ["ε"])
 ("Epsilon "         ["Ε"])
 ("zeta "            ["ζ"])
 ("Zeta "            ["Ζ"])
 ("eta "             ["η"])
 ("Eta "             ["Η"])
 ("theta "           ["θ"])
 ("Theta "           ["Θ"])
 ("iota "            ["ι"])
 ("Iota "            ["Ι"])
 ("kappa "           ["κ"])
 ("Kappa "           ["Κ"])
 ("lambda "          ["λ"])
 ("Lambda "          ["Λ"])
 ("lamda "           ["λ"])
 ("Lamda "           ["Λ"])
 ("mu "              ["μ"])
 ("Mu "              ["Μ"])
 ("nu "              ["ν"])
 ("Nu "              ["Ν"])
 ("xi "              ["ξ"])
 ("Xi "              ["Ξ"])
 ("omicron "         ["ο"])
 ("Omicron "         ["Ο"])
 ("pi "              ["π"])
 ("Pi "              ["Π"])
 ("rho "             ["ρ"])
 ("Rho "             ["Ρ"])
 ("sigma "           ["σ"])
 ("Sigma "           ["Σ"])
 ("tau "             ["τ"])
 ("Tau "             ["Τ"])
 ("upsilon "         ["υ"])
 ("Upsilon "         ["Υ"])
 ("phi "             ["φ"])
 ("Phi "             ["Φ"])
 ("chi "             ["χ"])
 ("Chi "             ["Χ"])
 ("psi "             ["ψ"])
 ("Psi "             ["Ψ"])
 ("omega "           ["ω"])
 ("Omega "           ["Ω"])
 ("digamma "         ["ϝ"])
 ("Digamma "         ["Ϝ"])
 ("san "             ["ϻ"])
 ("San "             ["Ϻ"])
 ("qoppa "           ["ϙ"])
 ("Qoppa "           ["Ϙ"])
 ("sampi "           ["ϡ"])
 ("Sampi "           ["Ϡ"])
 ("stigma "          ["ϛ"])
 ("Stigma "          ["Ϛ"])
 ("heta "            ["ͱ"])
 ("Heta "            ["Ͱ"])
 ("sho "             ["ϸ"])
 ("Sho "             ["Ϸ"])

 ;; Double-struck letters
 ("|A|"              ["𝔸"])
 ("|B|"              ["𝔹"])
 ("|C|"              ["ℂ"])
 ("|D|"              ["𝔻"])
 ("|E|"              ["𝔼"])
 ("|F|"              ["𝔽"])
 ("|G|"              ["𝔾"])
 ("|H|"              ["ℍ"])
 ("|I|"              ["𝕀"])
 ("|J|"              ["𝕁"])
 ("|K|"              ["𝕂"])
 ("|L|"              ["𝕃"])
 ("|M|"              ["𝕄"])
 ("|N|"              ["ℕ"])
 ("|O|"              ["𝕆"])
 ("|P|"              ["ℙ"])
 ("|Q|"              ["ℚ"])
 ("|R|"              ["ℝ"])
 ("|S|"              ["𝕊"])
 ("|T|"              ["𝕋"])
 ("|U|"              ["𝕌"])
 ("|V|"              ["𝕍"])
 ("|W|"              ["𝕎"])
 ("|X|"              ["𝕏"])
 ("|Y|"              ["𝕐"])
 ("|Z|"              ["ℤ"])
 ("|gamma|"          ["ℽ"])
 ("|Gamma|"          ["ℾ"])
 ("|pi|"             ["ℼ"])
 ("|Pi|"             ["ℿ"])

 ;; Types
 ("::"               ["∷"])

 ;; Quantifiers
 ("forall"           ["∀"])
 ("exists"           ["∃"])

 ;; Arrows
 ("->"               ["→"])
 ;; ("-->"              ["⟶"])
 ("<-"               ["←"])
 ;; ("<--"              ["⟵"])
 ;; ("<->"              ["↔"])
 ;; ("<-->"             ["⟷"])

 ("=>"               ["⇒"])
 ;; ("==>"              ["⟹"])
 ;; ("<="               ["⇐"])
 ;; ("<=="              ["⟸"])
 ;; ("<=>"              ["⇔"])
 ;; ("<==>"             ["⟺"])

 ;; ("|->"              ["↦"])
 ;; ("|-->"             ["⟼"])
 ;; ("<-|"              ["↤"])
 ;; ("<--|"             ["⟻"])

 ;; ("|=>"              ["⤇"])
 ;; ("|==>"             ["⟾"])
 ;; ("<=|"              ["⤆"])
 ;; ("<==|"             ["⟽"])

 ("~>"               ["⇝"])
 ;; ("~~>"              ["⟿"])
 ("<~"               ["⇜"])
 ;; ("<~~"              ["⬳"])

 ;; (">->"              ["↣"])
 ;; ("<-<"              ["↢"])
 ;; ("->>"              ["↠"])
 ;; ("<<-"              ["↞"])

 ;; (">->>"             ["⤖"])
 ;; ("<<-<"             ["⬻"])

 ;; ("<|-"              ["⇽"])
 ;; ("-|>"              ["⇾"])
 ;; ("<|-|>"            ["⇿"])

 ;; ("<-/-"             ["↚"])
 ;; ("-/->"             ["↛"])

 ;; ("<-|-"             ["⇷"])
 ;; ("-|->"             ["⇸"])
 ;; ("<-|->"            ["⇹"])

 ;; ("<-||-"            ["⇺"])
 ;; ("-||->"            ["⇻"])
 ;; ("<-||->"           ["⇼"])

 ;; ("-o->"             ["⇴"])
 ;; ("<-o-"             ["⬰"])

 ;; Boolean operators
 ;; ("not"              ["¬"])
 ("&&"               ["∧"])
 ("||"               ["∨"])

 ;; Relational operators
 ("=="               ["≡"])
 ("/="               ["≢" "≠"])
 ("<="               ["≤"])
 (">="               ["≥"])
 ("/<"               ["≮"])
 ("/>"               ["≯"])

 ;; Arithmetic
 ;; (" / "              [" ÷ "])
 (" * "              [" ⋅ "])

 ;; Containers / Collections
 ;; ("++"               ["⧺"])
 ;; ("+++"              ["⧻"])
 ;; ("|||"              ["⫴"])
 ;; ("empty"            ["∅"])
 ("elem"             ["∈"])
 ("notElem"          ["∉"])
 ("member"           ["∈"])
 ("notMember"        ["∉"])
 ("union"            ["∪"])
 ("intersection"     ["∩"])
 ("isSubsetOf"       ["⊆"])
 ("isProperSubsetOf" ["⊂"])

 ;; Other
 ;; ("<<"               ["≪"])
 ;; (">>"               ["≫"])
 ("<<<"              ["⋘"])
 (">>>"              ["⋙"])
 ("<|"               ["⊲"])
 ("|>"               ["⊳"])
 ("><"               ["⋈"])
 ;; ("mempty"           ["∅"])
 ("mappend"          ["⊕"])
 ;; ("<*>"              ["⊛"])
 (" . "              [" ∘ "])
 ("undefined"        ["⊥"])
 (":="               ["≔"])
 ("=:"               ["≕"])
 ("=def"             ["≝"])
 ("=?"               ["≟"])
 ("..."              ["…"])

 ;; Braces
 ;; ("[|"               ["〚"])
 ;; ("|]"               ["〛"])

 ;; Numeric subscripts
 ("_0 "              ["₀"])
 ("_1 "              ["₁"])
 ("_2 "              ["₂"])
 ("_3 "              ["₃"])
 ("_4 "              ["₄"])
 ("_5 "              ["₅"])
 ("_6 "              ["₆"])
 ("_7 "              ["₇"])
 ("_8 "              ["₈"])
 ("_9 "              ["₉"])

 ;; Numeric superscripts
 ("^0 "              ["⁰"])
 ("^1 "              ["¹"])
 ("^2 "              ["²"])
 ("^3 "              ["³"])
 ("^4 "              ["⁴"])
 ("^5 "              ["⁵"])
 ("^6 "              ["⁶"])
 ("^7 "              ["⁷"])
 ("^8 "              ["⁸"])
 ("^9 "              ["⁹"])
)
