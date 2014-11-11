;;; haskell-process-tests.el

;;; Code:

(require 'ert)
(require 'haskell-process)

(eval-when-compile (require 'cl)) ;; for tests with mock to pass...

(progn ;; HACK install package.el including for emacs-23.3, then install external el-mock dependency
  (when (version< emacs-version "24")
    (with-current-buffer (url-retrieve-synchronously "http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el")
      (save-excursion
        (goto-char (point-min))
        (kill-line 8)
        (eval-buffer))))
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-refresh-contents)
  (package-install 'el-mock))

(require 'el-mock)

(ert-deftest haskell-process-wrapper-command-function-identity ()
  "No wrapper, return directly the command."
  (should (equal '("ghci")
                 (progn
                   (custom-set-variables '(haskell-process-wrapper-function #'identity))
                   (apply haskell-process-wrapper-function (list '("ghci")))))))

(ert-deftest haskell-process-wrapper-function-non-identity ()
  "Wrapper as a string, return the wrapping command as a string."
  (should (equal '("nix-shell" "default.nix" "--command" "cabal\\ run")
                 (progn
                   (custom-set-variables '(haskell-process-wrapper-function (lambda (argv)
                                                                              (append '("nix-shell" "default.nix" "--command")
                                                                                      (list (shell-quote-argument argv))))))
                   (apply haskell-process-wrapper-function (list "cabal run"))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-ghci ()
  (should (equal '("Starting inferior GHCi process ghci ..." "dumses1" nil "ghci" "-ferror-spans")
                 (let ((haskell-process-path-ghci "ghci")
                       (haskell-process-args-ghci '("-ferror-spans")))
                   (custom-set-variables '(haskell-process-wrapper-function #'identity))
                   (mocklet (((haskell-session-name "dummy-session") => "dumses1"))
                     (haskell-process-compute-process-log-and-command "dummy-session" 'ghci))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-ghci ()
  (should (equal '("Starting inferior GHCi process ghci ..." "dumses1" nil "nix-shell" "default.nix" "--command" "ghci\\ -ferror-spans")
                 (let ((haskell-process-path-ghci "ghci")
                       (haskell-process-args-ghci '("-ferror-spans")))
                   (custom-set-variables '(haskell-process-wrapper-function
                                           (lambda (argv) (append (list "nix-shell" "default.nix" "--command" )
                                                             (list (shell-quote-argument (mapconcat 'identity argv " ")))))))
                   (mocklet (((haskell-session-name "dummy-session") => "dumses1"))
                     (haskell-process-compute-process-log-and-command "dummy-session" 'ghci))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-cabal-repl ()
  (should (equal '("Starting inferior `cabal repl' process using cabal ..." "dumses2" nil "cabal" "repl" "--ghc-option=-ferror-spans" "dumdum-session")
                 (let ((haskell-process-path-cabal      "cabal")
                       (haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans")))
                   (custom-set-variables '(haskell-process-wrapper-function #'identity))
                   (mocklet (((haskell-session-name "dummy-session2") => "dumses2")
                             ((haskell-session-target "dummy-session2") => "dumdum-session"))
                     (haskell-process-compute-process-log-and-command "dummy-session2" 'cabal-repl))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-cabal-repl ()
  (should (equal '("Starting inferior `cabal repl' process using cabal ..." "dumses2" nil "nix-shell" "default.nix" "--command" "cabal\\ repl\\ --ghc-option\\=-ferror-spans" "dumdum-session")
                 (let ((haskell-process-path-cabal      "cabal")
                       (haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans")))
                   (custom-set-variables '(haskell-process-wrapper-function
                                           (lambda (argv) (append (list "nix-shell" "default.nix" "--command" )
                                                             (list (shell-quote-argument (mapconcat 'identity argv " ")))))))
                   (mocklet (((haskell-session-name "dummy-session2") => "dumses2")
                             ((haskell-session-target "dummy-session2") => "dumdum-session"))
                     (haskell-process-compute-process-log-and-command "dummy-session2" 'cabal-repl))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-cabal-ghci ()
  (should (equal '("Starting inferior cabal-ghci process using cabal-ghci ..." "dumses3" nil "cabal-ghci")
                 (let ((haskell-process-path-ghci "ghci"))
                   (custom-set-variables '(haskell-process-wrapper-function #'identity))
                   (mocklet (((haskell-session-name "dummy-session3") => "dumses3"))
                     (haskell-process-compute-process-log-and-command "dummy-session3" 'cabal-ghci))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-cabal-ghci ()
  (should (equal '("Starting inferior cabal-ghci process using cabal-ghci ..." "dumses3" nil "nix-shell" "default.nix" "--command" "cabal-ghci")
                 (let ((haskell-process-path-ghci "ghci"))
                   (custom-set-variables '(haskell-process-wrapper-function
                                           (lambda (argv) (append (list "nix-shell" "default.nix" "--command" )
                                                             (list (shell-quote-argument (mapconcat 'identity argv " ")))))))
                   (mocklet (((haskell-session-name "dummy-session3") => "dumses3"))
                     (haskell-process-compute-process-log-and-command "dummy-session3" 'cabal-ghci))))))

(ert-deftest test-haskell-process--compute-process-log-and-command-cabal-dev ()
  (should (equal '("Starting inferior cabal-dev process cabal-dev -s directory/cabal-dev ..." "dumses4" nil "cabal-dev" "ghci" "-s" "directory/cabal-dev")
                 (let ((haskell-process-path-cabal-dev "cabal-dev"))
                   (custom-set-variables '(haskell-process-wrapper-function #'identity))
                   (mocklet (((haskell-session-name "dummy-session4")      => "dumses4")
                             ((haskell-session-cabal-dir "dummy-session4") => "directory"))
                     (haskell-process-compute-process-log-and-command "dummy-session4" 'cabal-dev))))))

(ert-deftest test-haskell-process--with-wrapper-compute-process-log-and-command-cabal-dev ()
  (should (equal '("Starting inferior cabal-dev process cabal-dev -s directory/cabal-dev ..." "dumses4" nil "run-with-docker" "cabal-dev\\ ghci\\ -s\\ directory/cabal-dev")
                 (let ((haskell-process-path-cabal-dev "cabal-dev"))
                   (custom-set-variables '(haskell-process-wrapper-function
                                           (lambda (argv) (append (list "run-with-docker")
                                                             (list (shell-quote-argument (mapconcat 'identity argv " ")))))))
                   (mocklet (((haskell-session-name "dummy-session4") => "dumses4")
                             ((haskell-session-cabal-dir "dummy-session4") => "directory"))
                     (haskell-process-compute-process-log-and-command "dummy-session4" 'cabal-dev))))))

;;; haskell-process-tests.el ends here
