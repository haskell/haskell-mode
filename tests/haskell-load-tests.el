;;; haskell-load-tests.el  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'haskell)

(ert-deftest do-cabal-no-process ()
  "Ensure that haskell-process-do-cabal can call cabal directly.

Redefine `shell-command' to just capture the command it's asked
to execute, and make sure it matches what we expected."
  (let (shell-call)
    (cl-letf (((symbol-function 'shell-command) (lambda (command &optional input-buffer output-buffer)
                                                  (setq shell-call command))))
      (haskell-process-do-cabal "help")
      (should (equal shell-call "cabal help")))))
