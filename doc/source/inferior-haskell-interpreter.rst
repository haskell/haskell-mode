Inferior Haskell interpreter
============================

The major mode ``inferior-haskell-mode`` provides support for
interacting with an inferior Haskell process based on ``comint-mode``.
So you can use most of the default shortcuts that come with the
comint based modes. For example ``M-x shell`` and ``M-x ielm``.

The comint based shell can be started using

+-------------------------+
|``M-x run-haskell``      |
|``M-x switch-to-haskell``|
+-------------------------+

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

+ The first preference is given to stack.
+ The second preferece is given to cabal.
+ The last preference is given to the default interpreter ghci.

The Haskell interpreter used by the inferior Haskell mode is
auto-detected by default, but is customizable via the

======================================= ================== =========================================
Interpreter (defcustom)                 Default Value      Default commandline arguments (defcustom)
======================================= ================== =========================================
``haskell-process-path-ghci``           ``ghci``           ``-ferror-spans``

``haskell-process-path-cabal``          ``cabal``          ``--ghc-option=-ferror-spans``

``haskell-process-path-stack``          ``stack``          ``--ghci-options=-ferror-spans``
                                                           ``--no-build``
                                                           ``--no-load``
======================================= ================== =========================================

How do I use a different interpreter?
-------------------------------------

If you are going to use a different interpreter (other than ghci) then set
the haskell-process-path-ghci to whatever you want (maybe hugs?). Also set
the commandline arguments to ``null`` or something based on your choice.
