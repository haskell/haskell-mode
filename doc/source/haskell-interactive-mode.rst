Haskell interactive mode
========================

An alternative mode providing a REPL via
GHCi sessions is called ``haskell-interactive-mode``, which
effectively replaces ``inferior-haskell-mode``, but comes with a
different set of features:

+ Separate sessions per Cabal project ``haskell-session.el``.
+ A new inferior Haskell process handling code ``haskell-process.el``.
+ New REPL implementation similiar to SLIME/IELM
+ Navigatable error overlays ``haskell-interactive-mode.el``.

With ``haskell-interactive-mode``, each Haskell source buffer is
associated with at most one GHCi session, so when you call
``haskell-process-load-file`` for a Haskell source buffer which has
no session associated yet, you're asked which GHCi session to create or
associate with.

Goto Error
----------

In a Haskell source buffer associated with a GHCi session, errors that
prevent the file from loading are highlighted with
``haskell-error-face``.  You can move between these error lines with

================================================
Key Binding         Function
===========         ============================
``M-n``             ``haskell-goto-next-error``
``M-p``             ``haskell-goto-prev-error``
``C-c M-p``         ``haskell-goto-first-error``
===========         ============================

Using GHCi 8+ or GHCi-ng
------------------------

If you use either of the above, then you can use these functions:

..  code-block:: scheme

    (define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
    (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

You have to load the module before it works, after that it will remember
for the current GHCi session.

Customizing
-----------

@cindex customizing
What kind of Haskell REPL ``haskell-interactive-mode`` will start up
depends on the value of ``haskell-process-type``. This can be one of the
symbols ``auto``, ``ghci``, ``cabal-repl``, ``cabal-new-repl``, or
``stack-ghci``. If it's ``auto``, the directory contents and available
programs will be used to make a best guess at the process type. The actual
process type will then determine which variables
``haskell-interactive-mode`` will access to determine the program to start
and its arguments:

@itemize
@item
If it's ``ghci``, ``haskell-process-path-ghci`` and
``haskell-process-args-ghci`` will be used.
@item
If it's ``cabal-repl``, ``haskell-process-path-cabal`` and
``haskell-process-args-cabal-repl``.
@item
If it's ``cabal-new-repl``, ``haskell-process-path-cabal`` and
``haskell-process-args-cabal-new-repl``.
@item
If it's ``stack-ghci``, ``haskell-process-path-stack`` and
``haskell-process-args-stack-ghci`` will be used.
@end itemize

With each of these pairs, the the ``haskell-process-path-...``
variable needs to be a string specifying the program path, or a list of
strings where the first element is the program path and the rest are
initial arguments.  The ``haskell-process-args-...`` is a list of
strings specifying (further) command-line arguments.

Haskell Interactive Mode Setup
------------------------------

The most straight-forward way to get setup with Interactive Mode is to
bind the right keybindings and set some customizations. This page
contains a good base setup.

To enable the minor mode which activates keybindings associated with interactive mode, use:

.. code-block:: scheme

    (require 'haskell-interactive-mode)
    (require 'haskell-process)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

Customizations
~~~~~~~~~~~~~~

This enables some handy and benign features.

.. code-block:: scheme

    (custom-set-variables
      '(haskell-process-suggest-remove-import-lines t)
      '(haskell-process-auto-import-loaded-modules t)
      '(haskell-process-log t))

Haskell-mode bindings
~~~~~~~~~~~~~~~~~~~~~

This gives the basic ways to start a session. In a Haskell buffer:

@itemize
@item
Run ``C-``` to make a REPL open, this will create a
session, start GHCi, and open the REPL.
@item
Or: run ``C-c C-l`` to load the file. This will first try to start a
session as the previous command does.
@item
Or: run any command which requires a running session. It will always
prompt to create one if there isn't one already for the current project.
@end itemize

.. code-section:: scheme

    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

Cabal-mode bindings
~~~~~~~~~~~~~~~~~~~

The below commands pretty much match the ones above, but are handy to
have in cabal-mode, too:

.. code-section:: scheme

    (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

GHCi process type
~~~~~~~~~~~~~~~~~

By default ``haskell-process-type`` is set to ``auto``. It is
smart enough to pick the right type based on your project structure and
installed tools, but in case something goes funky or you want to
explicitly set the process type and ignore the inferred type, you can
customize this setting by running ``M-x`` ``customize-variable``
``RET`` ``haskell-process-type`` ``RET``, or by setting the code:

.. code-section:: scheme

    (custom-set-variables
      '(haskell-process-type 'cabal-repl))

Here is a list of available process types:

+ ``ghci``
+ ``cabal-repl``
+ ``cabal-new-repl``
+ ``cabal-dev``
+ ``cabal-ghci``
+ ``stack-ghci``

Please, check the documentation for ``haskell-process-type`` to see how
the real type is guessed, when it's set to ``auto``.

Troubleshooting
~~~~~~~~~~~~~~~

Launching your GHCi process can fail when you're first getting setup,
depending on the type you choose. If it does fail to launch, switch to
the buffer ``*haskell-process-log*`` and see what's up. The buffer
contains a log of incoming/outgoing messages to the GHCi process.

Haskell Interactive Mode Tags Using GHCi
----------------------------------------

You can bind the following to use GHCi to find definitions of things:

.. code-section:: scheme

    (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)

The one problem with this approach is that if your code doesn't compile,
GHCi doesn't give any location info. So you need to make sure your code
compiles and the modules you want to jump to are loaded byte-compiled.

Note: I think that when you restart GHCi you lose location
information, even if you have the ``.o`` and ``.hi`` files lying around. I'm not
sure. But sometimes ``:i foo`` will give ``foo is defined in Bar`` rather
than ``foo is defined in /foo/Bar.hs:123:23``.

Alternatively, you can use tags generation, which doesn't require a
valid compile.

Tags Setup
~~~~~~~~~~

Make sure to install ``hasktags``.

.. code-section:: bash

    $ cabal install hasktags

Then add the customization variable to enable tags generation on save:

.. code-section:: scheme

    (custom-set-variables
      '(haskell-tags-on-save t))

And make sure ``hasktags`` is in your ``$PATH`` which Emacs can see.

Generating tags
~~~~~~~~~~~~~~~

Now, every time you run ``save-buffer`` (``C-x C-s``), there is a
hook that will run and generate Emacs @xref``Tags,,,emacs`` for the whole
project directory. The resulting file will be called ``TAGS``.

WARNING: You should be careful that your project root isn't your
home directory or something, otherwise it will traverse all the way
down and take an impossibly long time.

Jumping to tags
~~~~~~~~~~~~~~~

Bind the following keybinding:

.. code-section:: scheme

    (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

To jump to the location of the top-level identifier at point, run
``M-x`` ``haskell-mode-tag-find`` or ``M-.``.

Hybrid: GHCi and fallback to tags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use GHCi first and then if that fails to fallback to tags for jumping, use:

.. code-section:: scheme

    (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)


Troubleshooting tags
~~~~~~~~~~~~~~~~~~~~

Sometimes a ``TAGS`` file is deleted (by you or some other
process). Emacs will complain that it doesn't exist anymore. To
resolve this simply do ``M-x tags-reset-tags-tables``.

Sessions
--------

All commands in Haskell Interactive Mode work within a session. Consider
it like a “project” or a “solution” in popular IDEs. It tracks the root
of your project and an associated process and REPL.

Start a session
~~~~~~~~~~~~~~~

To start a session run the following steps:

@itemize
@item
Open some Cabal or Haskell file.
@item
Run ``C-``` to make a REPL open, this will create a session, start
GHCi, and open the REPL.
@item
Or: run ``C-c C-l`` to load the file. This will first try to start a
session as the previous command does.
@item
Or: run any command which requires a running session. It will always
prompt to create one if there isn't one already for the current project.
@end itemize

It will prompt for a Cabal directory and a current directory. It figures
out where the cabal directory is and defaults for the current directory,
so you should be able to just hit RET twice.

Switch a session
~~~~~~~~~~~~~~~~

Sometimes a particular file is used in two different
sessions/projects. You can run

``M-x haskell-session-change``

If it prompts you to make a new session, tell it no (that's a
bug). It will ask you to choose from a list of sessions.

Killing a session
~~~~~~~~~~~~~~~~~

To kill a session you can run

``M-x haskell-session-kill``

Which will prompt to kill all associated buffers, too. Hit `n` to
retain them.

Alternatively, you can switch to the REPL and just kill the buffer
normally with ``C-x k RET``. It will prompt

``Kill the whole session (y or n)?``

You can choose ``y`` to kill the session itself, or ``n`` to just
kill the REPL buffer. You can bring it back with ``M-x``
``haskell-interactive-bring``.

Menu
~~~~

To see a list of all sessions you have open with some simple
statistics about memory usage, etc. run

@example
    M-x haskell-menu
@end example

For example:

@example
    foo  14648 08:21:42 214MB /path/to/fpco/foo/  /path/to/fpco/foo/ ghci
    bar  29119 00:22:03 130MB /path/to/bar/       /path/to/bar/      ghci
    mu   22575 08:48:20 73MB  /path/to/fpco/mu/   /path/to/fpco/mu/  ghci
@end example


Compiling
---------

There are a bunch of ways to compile Haskell modules. This page covers
a few of them.

Load into GHCi
~~~~~~~~~~~~~~

To compile and load a Haskell module into GHCi, run the following

``M-x haskell-process-load``

Or ``C-c C-l``. You'll see any compile errors in the REPL window.

Build the Cabal project
~~~~~~~~~~~~~~~~~~~~~~~

To compile the whole Cabal project, run the following

``M-x haskell-process-cabal-build``

Or ``C-c C-c``. You'll see any compile errors in the REPL window.

Reloading modules
~~~~~~~~~~~~~~~~~

To reload the current module, even when you're in other modules, you can
run ``C-u M-x`` ``haskell-process-load-or-reload`` or ``C-u C-c
C-l``. It will now reload that module whenever you run ``C-c C-l`` in
the future from whatever module you're in. To disable this mode, just
run ``C-u C-c C-l`` again.

Jumping to compile errors
~~~~~~~~~~~~~~~~~~~~~~~~~

You can use the standard compile error navigation function ``C-x ``` —
jump to the next error.

Or you can move your cursor to an error in the REPL and hit ``RET`` to
jump to it.

Auto-removing imports
~~~~~~~~~~~~~~~~~~~~~

If the customization variable
``haskell-process-suggest-remove-import-lines`` is enabled.

.. code-section:: scheme

    (custom-set-variables
      '(haskell-process-suggest-remove-import-lines t))


Building and loading modules which output warnings like,

@example
    Warning: The import of `Control.Monad' is redundant
      except perhaps to import instances from `Control.Monad'
    To import instances alone, use: import Control.Monad()
@end example

will prompt the user with

@example
> The import line `Control.Monad' is redundant. Remove? (y, n, c: comment out)
@end example

If you answer

@itemize
@item
``y``: it will delete the import, but leave the empty line remaining
(this avoids messing with line positions in subsequent error messages).
@item
``n``: it will leave the import.
@item
``c``: it will comment out the import (this is handy for when you just
want to temporarily hide an import).
@end itemize

Auto-adding of modules to import
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Enable the customization variable
``haskell-process-suggest-hoogle-imports``.

.. code-section:: scheme

    (custom-set-variables
      '(haskell-process-suggest-hoogle-imports t))


Whenever GHC says something is not in scope, it will hoogle that
symbol. If there are results, it will prompt to add one of the modules
from Hoogle's results.

You need to make sure you've generated your Hoogle database properly.

Auto-adding of extensions
~~~~~~~~~~~~~~~~~~~~~~~~~

It you use an extension which is not enabled, GHC will often inform
you. For example, if you write:

@example
newtype X a = X (IO a)
  deriving (Monad)
@end example


Then you'll see a message like:

@example
    x.hs:13:13: Can't make a derived instance of `Monad X': …
          `Monad' is not a derivable class
          Try -XGeneralizedNewtypeDeriving for GHC's newtype-deriving extension
        In the newtype declaration for `X'
@end example

This ``-XFoo`` pattern will be picked up and you will be prompted:

@example
> Add `@``-# LANGUAGE GeneralizedNewtypeDeriving #-@``` to the top of the
> file? (y or n)
@end example

If you answer `y`, it will temporarily jump to the buffer and it to
the top of the file.

Orphan instances
~~~~~~~~~~~~~~~~

If GHC complains about orphan instances, you usually are doing it
intentionally, so it prompts to add ``-fno-warn-orphans`` to the top of
the file with an ``OPTIONS`` pragma.

Auto-adding of dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~

When doing a build, you will sometimes get a message from GHC like:

@example
    src/ACE/Tokenizer.hs:11:18: Could not find module `Data.Attoparsec.Text' …
        It is a member of the hidden package `attoparsec-0.11.1.0'.
@end example

This message contains all the necessary information to add this to
your .cabal file, so you will be prompted to add it to your .cabal
file:

@example
    Add `attoparsec' to ace.cabal? (y or n)  y
@end example

If you hit ``y``, it will prompt with this:

@example
    attoparsec >= 0.11.1.0
@end example

Which you can edit (e.g. do some PVP decision or remove constraints
entirely), and then it will open up your ``.cabal`` file and go
through each section:

@example
    Add to library? (y or n)  y
@end example

This will add it to the top of the ``build-depends`` field in your
library section. If you have any executables, it will go through each
of those, prompting, too.

Now you can rebuild with ``C-c C-c`` again.

Haskell Interactive Mode REPL
-----------------------------

When GHCi has been launched, it works on a read-eval-print basis. So
you will be presented with the prompt:

@example
    The lambdas must flow.
    Changed directory: /path/to/your/project/
    λ>
@end example

Changing REPL target
~~~~~~~~~~~~~~~~~~~~

@findex haskell-session-change-target
@vindex haskell-interactive-mode-hook

With ``haskell-session-change-target`` you can change the target for
REPL session.


After REPL session started, in ``haskell-interactive-mode`` buffer invoke the
``haskell-session-change-target`` and select from available targets for

@cindex testing
- Testing

@cindex benchmarking
- Benchmark

- Executable

- Library

Answer ``yes'' to restart the session and run your tests, benchmarks, executables.


TODO/WRITEME


Bringing the REPL
~~~~~~~~~~~~~~~~~

If you don't know where the REPL buffer is, you can always bring it
with:

@example
    M-x haskell-interactive-bring
@end example

Or ``C-```.

Evaluating expressions
~~~~~~~~~~~~~~~~~~~~~~

To evaluate expressions, simply type one out and hit `RET`.

@example
    λ> 123
    123
@end example

Evaluating multiline expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHCi features two ways to evaluate multiline expressions. You can use
``:set +m`` to
enable @uref``https://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html#multiline-input,
multiline input`` for all expressions, or you can wrap your expression in
``:@```` and ``:@```` (they have to be on their own lines).

The prompt will change to indicate that you're inputting a multiline
expression:

@example
λ> :@``
λ| let a = 10
λ|     b = 20
λ|     c = 30
λ| :@``
@end example

You can also simulate multiline mode by having your input contain
newline characters. You can input a literal newline character with
``C-q C-j``, or you can use:

@example
    M-x haskell-interactive-mode-newline-indent
@end example

which is bound to ``C-j``. This command indents after the newline. You
can simulate the above example like so:

@example
λ> let a = 10
       b = 20
       c = 30
@end example

Type of expressions
~~~~~~~~~~~~~~~~~~~

You can use normal ``:type`` which is part of GHCi to get the type of
something:

@example
    λ> :t id
    id :: a -> a
@end example

But you can also just write out the value directly,

@example
    λ> id
    id :: a -> a
@end example

and because there's no ``Show`` instance for ``(a -> a)``. This would
normally yield a compile error:

@example
    No instance for (Show (a0 -> a0))
      arising from a use of `print'
    Possible fix: add an instance declaration for (Show (a0 -> a0))
    In a stmt of an interactive GHCi command: print it
@end example

It will run ``:type id`` in the background and print out the
result. The same is true for ambiguous things:

@example
    λ> :t read "a"
    read "a" :: Read a => a
@end example

Because this would normally be an ambiguous constraint:

@example
    Ambiguous type variable `a0' in the constraint:
      (Read a0) arising from a use of `read'
    Probable fix: add a type signature that fixes these type variable(s)
    In the expression: read \"a\"
    In an equation for `it': it = read \"a\"
@end example

Which is less useful than just printing the type out.

You can disable this behaviour by disabling the customization option:

.. code-section:: scheme

    (custom-set-variables
      '(haskell-interactive-types-for-show-ambiguous nil))


Printing mode
~~~~~~~~~~~~~

You can choose between printing modes used for the results of
evaluating expressions. To do that, configure the variable
``haskell-interactive-mode-eval-mode``. Example:

.. code-section:: scheme

    (setq haskell-interactive-mode-eval-mode 'haskell-mode)



A handy function you can use is:

.. code-section:: scheme

    (defun haskell-interactive-toggle-print-mode ()
      (interactive)
      (setq haskell-interactive-mode-eval-mode
            (intern
             (ido-completing-read "Eval result mode: "
                                  '("fundamental-mode"
                                    "haskell-mode"
                                    "espresso-mode"
                                    "ghc-core-mode"
                                    "org-mode")))))


(Add whichever modes you want to use.)

And then run

@example
    M-x haskell-interactive-toggle-print-mode
@end example

Or ``C-c C-v``:

.. code-section:: scheme

    (define-key haskell-interactive-mode-map (kbd "C-c C-v")
                'haskell-interactive-toggle-print-mode)


There you can choose `haskell-mode`, for example, to pretty print the
output as Haskell.

Presentations
~~~~~~~~~~~~~

If you have the ``present`` package installed, you can use the following
syntax to print anything which is an instance of ``Data``:

@example
    λ> :present 123
    123
@end example

It will print data structures lazily:

@example
    λ> :present [1..]
    [1
    ,[Integer]]
@end example

It shows types when there is an unevaluated field in a constructor. You
can click the ``[Integer]`` or press ``RET`` on it to expand
further:

@example
    λ> :present [1..]
    [1
    ,2
    ,[Integer]]
@end example

Etc. Remember: this only works for instances of ``Data.Data.Data``.

History
~~~~~~~

A history is maintained for the duration of the REPL buffer. To go up
and down in the history, run ``M-p`` for previous and ``M-n`` for
next.

Cancelling commands
~~~~~~~~~~~~~~~~~~~

To cancel a running REPL command, run ``C-c C-c``.

Clear the REPL
~~~~~~~~~~~~~~

Run ``C-c C-k`` to clear the REPL.


Trick: Put Interactive REPL in Separate Frame
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following ``create-haskell-interactive-frame`` is a quick hack to
move the repl to a separate frame, for those that want a more
predictable layout of windows in Emacs.

.. code-section:: scheme

    (defun create-unfocused-frame ()
      (let*
        ((prv (window-frame))
         (created (make-frame)))
        (select-frame-set-input-focus prv) created))

    (defun create-haskell-interactive-frame ()
      (interactive)
      (haskell-interactive-bring)
      (create-unfocused-frame)
      (delete-window))

Troubleshooting
~~~~~~~~~~~~~~~

If the REPL ever goes funny, you can clear the command queue via:

@example
    M-x haskell-process-clear
@end example

Alternatively, you can just restart the process:

@example
    M-x haskell-process-restart
@end example

You can also switch to the buffer ``*haskell-process-log*``, which can
be enabled and disabled with the customization variable
`haskell-process-log`, to see what the cause of your troubles are.

If the process fails and nothing unusual is in the process log, the
following command can dump the ``haskell-process`` state:

@example
    M-: (haskell-process)
@end example

The output can be copied from the ``*Messages*`` buffer.

Haskell Interactive Mode Querying
---------------------------------

There a few ways GHCi lets you query information about your code.

Get identifer type
~~~~~~~~~~~~~~~~~~

To print the type of the top-level identifier at point in the REPL and
in the message buffer, run the following command:

@example
    M-x haskell-process-do-type
@end example

or ``C-c C-t``.

Insert identifier's type as type signature
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To print the type of the top-level identifier at point, run the
following command:

@example
    C-u M-x haskell-process-do-type
@end example

or ``C-u C-c C-t``.

Get identifier info
~~~~~~~~~~~~~~~~~~~

To print the info of the identifier at point, run the following
command:

@example
    M-x haskell-process-do-info
@end example

or ``C-c C-i``.

Presentation mode
~~~~~~~~~~~~~~~~~

When using ``C-c C-i`` or ``C-c C-t`` it will open a buffer in
haskell-presentation-mode. You can hit ``q`` to close the buffer.

But you can also continue to use ``C-c C-i`` inside the buffer to
drill further down data types and classes.

E.g. if you go to ``Ord`` in your code buffer and ``C-c C-i``, it
will popup a buffer containing

@example
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  	-- Defined in `GHC.Classes'
@end example

And all the instances of that class. But then you can also move your
cursor to ``Ordering`` and hit ``C-c C-i`` again to get another
popup:

@example
data Ordering = LT | EQ | GT 	-- Defined in `GHC.Types'
instance Bounded Ordering -- Defined in `GHC.Enum'
instance Enum Ordering -- Defined in `GHC.Enum'
instance Eq Ordering -- Defined in `GHC.Classes'
instance Ord Ordering -- Defined in `GHC.Classes'
instance Read Ordering -- Defined in `GHC.Read'
instance Show Ordering -- Defined in `GHC.Show'
@end example

And so on. It's a very good way of exploring a new codebase.

Browse import's module
~~~~~~~~~~~~~~~~~~~~~~

To print all exported identifiers of the module imported by the import
line at point, run the following command:

@example
    M-x haskell-process-do-info
@end example

or ``C-c C-i``. It will print all exports by running ``:browse
The.Module`` in the GHCi process.

Haskell Interactive Mode Cabal integration
------------------------------------------

There's some integration with Cabal in Haskell Interactive Mode. Once
you've started a session, the features below are available.

Cabal building
~~~~~~~~~~~~~~

The most common Cabal action is building, so that has a specific command:

@example
    M-x haskell-process-cabal-build
@end example

Or ``C-c C-c``. When building, it will hide unneccessary output.

For example, to build the `ace` package, the output is simply:

@example
    Compiling: ACE.Types.Tokens
    Compiling: ACE.Combinators
    Compiling: ACE.Tokenizer
    Compiling: ACE.Parsers
    Compiling: ACE.Pretty
    Compiling: ACE
    Complete: cabal build (0 compiler messages)
@end example

Whereas the complete output is normally:

@example
    Building ace-0.5...
    Preprocessing library ace-0.5...
    [4 of 9] Compiling ACE.Types.Tokens ( src/ACE/Types/Tokens.hs, dist/build/ACE/Types/Tokens.o )
    [5 of 9] Compiling ACE.Combinators  ( src/ACE/Combinators.hs, dist/build/ACE/Combinators.o ) [ACE.Types.Tokens changed]
    [6 of 9] Compiling ACE.Tokenizer    ( src/ACE/Tokenizer.hs, dist/build/ACE/Tokenizer.o ) [ACE.Types.Tokens changed]
    [7 of 9] Compiling ACE.Parsers      ( src/ACE/Parsers.hs, dist/build/ACE/Parsers.o )
    [8 of 9] Compiling ACE.Pretty       ( src/ACE/Pretty.hs, dist/build/ACE/Pretty.o )
    [9 of 9] Compiling ACE              ( src/ACE.hs, dist/build/ACE.o ) [ACE.Tokenizer changed]
    In-place registering ace-0.5...
@end example

Which is considerably more verbose but rarely useful or interesting.

Arbitrary cabal commands
~~~~~~~~~~~~~~~~~~~~~~~~

To run an arbitrary Cabal command:

@example
    C-u M-x haskell-process-cabal
@end example

Or run ``C-u C-c c``.

It will prompt for an input, so you can write ``configure -fdev``,
for example.

Completing cabal commands
~~~~~~~~~~~~~~~~~~~~~~~~~

To run some common Cabal commands, just run:

@example
    M-x haskell-process-cabal
@end example

Or ``C-c c``. This is commonly used to do ``install``,
``haddock``, ``configure``, etc.

Haskell Interactive Mode Debugger
---------------------------------

There is limited support for debugging in GHCi. Haskell Interactive Mode
provides an interface for interacting with this.

Opening the debug buffer
~~~~~~~~~~~~~~~~~~~~~~~~

To open the debug buffer run the following command from any buffer
associated with a session:

@example
    M-x haskell-debug
@end example

It will open a buffer that looks like this:

@example
    Debugging haskell

    You have to load a module to start debugging.

    g - refresh

    Modules

    No loaded modules.
@end example


Loading modules
~~~~~~~~~~~~~~~

To debug anything you need to load something into GHCi. Switch to a
normal file, for example:

@example
main = do putStrLn "Hello!"
          putStrLn "World"
@end example

and load it into GHCi (``C-c C-l``). Now when you hit ``g``
(to refresh) in the debugging buffer, you'll see something like:

@example

    Debugging haskell

    b - breakpoint, g - refresh

    Context

    Not debugging right now.

    Breakpoints

    No active breakpoints.

    Modules

    Main - hello.hs
@end example

Setting a breakpoint
~~~~~~~~~~~~~~~~~~~~

To set a breakpoint hit ``b`` in the debugger buffer. It will prompt
for a name. Enter ``main`` and hit ``RET``.

Now the buffer will look like this:

@example
    Debugging haskell

    s - step into an expression, b - breakpoint
    d - delete breakpoint, g - refresh

    Context

    Not debugging right now.

    Breakpoints

    0 - Main (1:8)

    Modules

    Main - hello.hs
@end example

Start stepping
~~~~~~~~~~~~~~

Hit ``s`` to step through an expression: it will prompt for an
expression to evaluate and step through. Enter ``main`` and hit
``RET``. Now the buffer will look like this:

@example
    Debugging haskell

    s - step into an expression, b - breakpoint
    d - delete breakpoint, a - abandon context, c - continue
    p - previous step, n - next step
    g - refresh

    Context

    main - hello.hs (stopped)

    do putStrLn "Hello!"
       putStrLn "World"

    _result :: IO () = _

       1 do putStrLn "Hello!" putStrLn "World"

    Breakpoints

    0 - Main (1:8)

    Modules

    Main - hello.hs
@end example

What we see here is the current expression being evaluated:

@example
do putStrLn "Hello!"
   putStrLn "World"
@end example

And we see the type of it:

@example
_result :: IO () = _
@end example

And we see a backtrace of steps so far:

@example
1 do putStrLn "Hello!" putStrLn "World"
@end example

Continue stepping
~~~~~~~~~~~~~~~~~

To continue stepping, just hit ``s`` again. Now the context will change
to:

@example
main - hello.hs (stopped)

putStrLn "Hello!"

_result :: IO () = _

   1 do putStrLn "Hello!" putStrLn "World"
@end example


Hitting ``s`` once more, we see the context change to:

@example
putStrLn "World"

_result :: IO () = _

   2 putStrLn "Hello!"
   1 do putStrLn "Hello!" putStrLn "World"
@end example

Finally hitting ``s`` again will say "Computation finished". Hitting
``s`` a final time will change the display back to:

@example
    Debugging haskell

    s - step into an expression, b - breakpoint
    d - delete breakpoint, g - refresh

    Context

    Finished debugging.

       2 putStrLn "Hello!"
       1 do putStrLn "Hello!" putStrLn "World"

    Breakpoints

    1 - Main (1:8)

    Modules

    Main - hello.hs
@end example

And you're done debugging.
