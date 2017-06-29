Code completion
===============

can get more precise completions with ``haskell-interactive-mode``.
In interactive mode completion candidates are produced by querying
GHCi REPL.

If ``haskell-interactive-mode`` is enabled and working Haskell mode
provides completions for import statements taking into account
currently loaded and available packages.  Also it completes symbols
querying REPL with ``:complete`` command, hence completion
candidate list also includes symbols from imported modules.

Unfortunatelly, it is not possible to provide candidates for
identifiers defined locally in ``let`` and ``where`` blocks even
in interactive mode.  But if you're using
`company-mode<http://company-mode.github.io/>`_ you can override
``company-backends`` variable for Haskell buffers to combine
completion candidates from completion-at-point function
(``company-capf`` backend) and dynamic abbrevs.
``company-mode`` provides special backend for dabbrev code
completions, namely ``company-dabbrev-code``.  To combine
completions from diffrent backends you can create grouped backends, it
is very easy — a grouped backend is just a list of backends, for
example:

.. code-block:: scheme

    (add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))))


If you use a GHCi version prior to 8.0.1 you might want to set
``haskell-completions-complete-operators`` to ``nil``, if you
experience major slowdown while trying to complete after an Haskell
operator (See `GHC-Bug 10576<https://ghc.haskell.org/trac/ghc/ticket/10576>`_).
