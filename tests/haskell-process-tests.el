;;; haskell-process-tests.el

;;; Code:

(require 'ert)
(require 'haskell-process)

;; HACK how to install deps in haskell-mode
(progn (require 'package)
       (package-initialize)
       (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
       (package-refresh-contents)
       (package-install 'el-mock))

(require 'el-mock)

(ert-deftest haskell-process-stringify-cmd-no-arg ()
  "No wrapper, return directly the command (between string quote)."
  (should (equal "run"
                 (let ((haskell-process-wrapper nil))
                   (haskell-process-stringify-cmd "run")))))

(ert-deftest haskell-process-stringify-cmd-with-args ()
  "No wrapper, return directly the command."
  (should (equal "run -a b -c d"
                 (let ((haskell-process-wrapper nil))
                   (haskell-process-stringify-cmd "run" '("-a" "b" "-c" "d"))))))

(ert-deftest haskell-process-wrapper-command-nil ()
  "No wrapper, return directly the command."
  (should (equal '("ghci")
                 (let ((haskell-process-wrapper nil))
                   (haskell-process-wrapper-command "ghci")))))

(ert-deftest haskell-process-wrapper-command-with-string ()
  "Wrapper as a string, return the wrapping command as a string."
  (should (equal '("nix-shell" "cabal run")
                 (let ((haskell-process-wrapper "nix-shell"))
                   (haskell-process-wrapper-command "cabal run")))))

(ert-deftest haskell-process-wrapper-command-with-string-2 ()
  "Wrapper as a string, return the wrapping command as a string."
  (should (equal '("nix-shell" "cabal repl")
                 (let ((haskell-process-wrapper "nix-shell"))
                   (haskell-process-wrapper-command "cabal" '("repl"))))))

(ert-deftest haskell-process-wrapper-command-with-repeat-string ()
  "Wrapper as a list of string, return the wrapping command as a string."
  (should (equal '("nix-shell" "default.nix" "--command" "cabal build")
                 (let ((haskell-process-wrapper '("nix-shell" "default.nix" "--command")))
                   (haskell-process-wrapper-command "cabal" '("build"))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-ghci ()
  (should (equal '("Starting inferior GHCi process ghci ..." "dumses1" nil "ghci" "-ferror-spans")
                 (let ((haskell-process-path-ghci "ghci")
                       (haskell-process-args-ghci '("-ferror-spans"))
                       (haskell-process-wrapper   nil))
                   (mocklet (((haskell-session-name "dummy-session") => "dumses1"))
                     (haskell-process-compute-process-log-and-command "dummy-session" 'ghci))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-ghci ()
  (should (equal '("Starting inferior GHCi process ghci ..." "dumses1" nil "nix-shell" "default.nix" "--command" "ghci -ferror-spans")
                 (let ((haskell-process-path-ghci "ghci")
                       (haskell-process-args-ghci '("-ferror-spans"))
                       (haskell-process-wrapper   '("nix-shell" "default.nix" "--command")))
                   (mocklet (((haskell-session-name "dummy-session") => "dumses1"))
                     (haskell-process-compute-process-log-and-command "dummy-session" 'ghci))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-cabal-repl ()
  (should (equal '("Starting inferior `cabal repl' process using cabal ..." "dumses2" nil "cabal" "repl" "--ghc-option=-ferror-spans" "dumdum-session")
                 (let ((haskell-process-path-cabal      "cabal")
                       (haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"))
                       (haskell-process-wrapper         nil))
                   (mocklet (((haskell-session-name "dummy-session2") => "dumses2")
                             ((haskell-session-target "dummy-session2") => "dumdum-session"))
                     (haskell-process-compute-process-log-and-command "dummy-session2" 'cabal-repl))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-cabal-repl ()
  (should (equal '("Starting inferior `cabal repl' process using cabal ..." "dumses2" nil "nix-shell" "default.nix" "--command" "cabal repl --ghc-option=-ferror-spans" "dumdum-session")
                 (let ((haskell-process-path-cabal      "cabal")
                       (haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"))
                       (haskell-process-wrapper         '("nix-shell" "default.nix" "--command")))
                   (mocklet (((haskell-session-name "dummy-session2") => "dumses2")
                             ((haskell-session-target "dummy-session2") => "dumdum-session"))
                     (haskell-process-compute-process-log-and-command "dummy-session2" 'cabal-repl))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-cabal-ghci ()
  (should (equal '("Starting inferior cabal-ghci process using cabal-ghci ..." "dumses3" nil "cabal-ghci")
                 (let ((haskell-process-path-ghci "ghci")
                       (haskell-process-wrapper   nil))
                   (mocklet (((haskell-session-name "dummy-session3") => "dumses3"))
                     (haskell-process-compute-process-log-and-command "dummy-session3" 'cabal-ghci))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-cabal-ghci ()
  (should (equal '("Starting inferior cabal-ghci process using cabal-ghci ..." "dumses3" nil "nix-shell" "default.nix" "--command" "cabal-ghci")
                 (let ((haskell-process-path-ghci "ghci")
                       (haskell-process-wrapper   '("nix-shell" "default.nix" "--command")))
                   (mocklet (((haskell-session-name "dummy-session3") => "dumses3"))
                     (haskell-process-compute-process-log-and-command "dummy-session3" 'cabal-ghci))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-cabal-dev ()
  (should (equal '("Starting inferior cabal-dev process cabal-dev -s directory/cabal-dev ..." "dumses4" nil "cabal-dev" "ghci" "-s" "directory/cabal-dev")
                 (let ((haskell-process-path-cabal-dev "cabal-dev")
                       (haskell-process-wrapper        nil))
                   (mocklet (((haskell-session-name "dummy-session4")      => "dumses4")
                             ((haskell-session-cabal-dir "dummy-session4") => "directory"))
                     (haskell-process-compute-process-log-and-command "dummy-session4" 'cabal-dev))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-cabal-dev ()
  (should (equal '("Starting inferior cabal-dev process cabal-dev -s directory/cabal-dev ..." "dumses4" nil "run-with-docker" "cabal-dev ghci -s directory/cabal-dev")
                 (let ((haskell-process-path-cabal-dev "cabal-dev")
                       (haskell-process-wrapper        "run-with-docker"))
                   (mocklet (((haskell-session-name "dummy-session4") => "dumses4")
                             ((haskell-session-cabal-dir "dummy-session4") => "directory"))
                     (haskell-process-compute-process-log-and-command "dummy-session4" 'cabal-dev))))))

;;; haskell-process-tests.el ends here
