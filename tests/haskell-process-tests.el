;;; haskell-process-tests.el

;;; Code:

(require 'ert)
(require 'el-mock)

(require 'haskell-process)

(ert-deftest haskell-session-wrapper-command-function-identity ()
  "No wrapper, return directly the command."
  (should (equal '("ghci")
                 (progn
                   (custom-set-variables '(haskell-session-wrapper-function #'identity))
                   (apply haskell-session-wrapper-function (list '("ghci")))))))

(ert-deftest haskell-session-wrapper-function-non-identity ()
  "Wrapper as a string, return the wrapping command as a string."
  (should (equal '("nix-shell" "default.nix" "--command" "cabal\\ run")
                 (progn
                   (custom-set-variables '(haskell-session-wrapper-function (lambda (argv)
                                                                              (append '("nix-shell" "default.nix" "--command")
                                                                                      (list (shell-quote-argument argv))))))
                   (apply haskell-session-wrapper-function (list "cabal run"))))))

(ert-deftest test-haskell-session--compute-process-log-and-command-ghci ()
  (should (equal '("Starting inferior GHCi process ghci ..." "dumses1" nil "ghci" "-ferror-spans")
                 (let ((haskell-session-path-ghci "ghci")
                       (haskell-session-args-ghci '("-ferror-spans")))
                   (custom-set-variables '(haskell-session-wrapper-function #'identity))
                   (mocklet (((haskell-session-name "dummy-session") => "dumses1"))
                     (haskell-session-compute-process-log-and-command "dummy-session" 'ghci))))))

(ert-deftest test-haskell-session--with-wrapper-compute-process-log-and-command-ghci ()
  (should (equal '("Starting inferior GHCi process ghci ..." "dumses1" nil "nix-shell" "default.nix" "--command" "ghci\\ -ferror-spans")
                 (let ((haskell-session-path-ghci "ghci")
                       (haskell-session-args-ghci '("-ferror-spans")))
                   (custom-set-variables '(haskell-session-wrapper-function
                                           (lambda (argv) (append (list "nix-shell" "default.nix" "--command" )
                                                             (list (shell-quote-argument (mapconcat 'identity argv " ")))))))
                   (mocklet (((haskell-session-name "dummy-session") => "dumses1"))
                     (haskell-session-compute-process-log-and-command "dummy-session" 'ghci))))))

(ert-deftest test-haskell-session--compute-process-log-and-command-cabal-repl ()
  (should (equal '("Starting inferior `cabal repl' process using cabal ..." "dumses2" nil "cabal" "repl" "--ghc-option=-ferror-spans" "dumdum-session")
                 (let ((haskell-session-path-cabal      "cabal")
                       (haskell-session-args-cabal-repl '("--ghc-option=-ferror-spans")))
                   (custom-set-variables '(haskell-session-wrapper-function #'identity))
                   (mocklet (((haskell-session-name "dummy-session2") => "dumses2")
                             ((haskell-session-target "dummy-session2") => "dumdum-session"))
                     (haskell-session-compute-process-log-and-command "dummy-session2" 'cabal-repl))))))

(ert-deftest test-haskell-session--with-wrapper-compute-process-log-and-command-cabal-repl ()
  (should (equal '("Starting inferior `cabal repl' process using cabal ..." "dumses2" nil "nix-shell" "default.nix" "--command" "cabal\\ repl\\ --ghc-option\\=-ferror-spans\\ dumdum-session")
                 (let ((haskell-session-path-cabal      "cabal")
                       (haskell-session-args-cabal-repl '("--ghc-option=-ferror-spans")))
                   (custom-set-variables '(haskell-session-wrapper-function
                                           (lambda (argv) (append (list "nix-shell" "default.nix" "--command" )
                                                             (list (shell-quote-argument (mapconcat 'identity argv " ")))))))
                   (mocklet (((haskell-session-name "dummy-session2") => "dumses2")
                             ((haskell-session-target "dummy-session2") => "dumdum-session"))
                     (haskell-session-compute-process-log-and-command "dummy-session2" 'cabal-repl))))))


;;; haskell-process-tests.el ends here
