;; unit tests for haskell-utils.el

(require 'ert)

(require 'haskell-utils ) ;; implementation under test

(ert-deftest haskell-utils-unqualify-op ()
  (should (string= (haskell-utils-unqualify-op "++")   "++"))
  (should (string= (haskell-utils-unqualify-op ":-->") ":-->"))
  (should (string= (haskell-utils-unqualify-op "<$>")  "<$>"))
  (should (string= (haskell-utils-unqualify-op "<*>")  "<*>"))
  (should (string= (haskell-utils-unqualify-op ">>=")  ">>="))
  (should (string= (haskell-utils-unqualify-op "~>")   "~>"))
  (should (string= (haskell-utils-unqualify-op ":+:")  ":+:"))

  (should (string= (haskell-utils-unqualify-op "Foo.:+:")           ":+:"))
  (should (string= (haskell-utils-unqualify-op "Control.Arrrow.~>") "~>"))
  (should (string= (haskell-utils-unqualify-op "Data.List.++")      "++"))
  (should (string= (haskell-utils-unqualify-op "L.++")              "++"))
  (should (string= (haskell-utils-unqualify-op "L.++")              "++"))
  (should (string= (haskell-utils-unqualify-op "GHC.Base.*")        "*")))

