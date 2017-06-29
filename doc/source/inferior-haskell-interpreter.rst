Inferior Haskell interpreter
============================

The major mode ``inferior-haskell-mode`` provides support for
interacting with an inferior Haskell process based on ``comint-mode``.

By default the ``haskell-mode-map`` keymap is setup to use this mode:

===========  ==============================
Key Binding  Function
===========  ==============================
``C-c C-z``  ``switch-to-haskell``
``C-c C-b``  ``switch-to-haskell``
``C-c C-l``  ``inferior-haskell-load-file``
``C-c C-t``  ``inferior-haskell-type``
``C-c C-i``  ``inferior-haskell-info``
===========  ==============================


The Haskell interpreter used by the inferior Haskell mode is
auto-detected by default, but is customizable via the
``haskell-program-name`` variable.

Currently, GHCi and Hugs are support as Haskell interpreter.

**TODO/WRITEME**
