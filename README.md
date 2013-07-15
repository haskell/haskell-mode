Haskell Mode for Emacs
----------------------

This is the Haskell mode package for Emacs.  Its use should be mostly
self-explanatory if you're accustomed to Emacs.

When Emacs is started up, it normally runs a file called `~/.emacs`
(or `~/.emacs.d/init.el`), `~` standing for your home directory.  This
file should contain all of your personal customisations written as a
series of Emacs Lisp commands.  In order to install the Haskell mode, you
have to tell Emacs where to find it.  This is done by adding some
commands to the init file.

Installation
------------

There are many ways to install `haskell-mode`. Pick the one that
you're most comfortable with.

### package.el

`package.el` is the built-in package manager in Emacs 24.x. On Emacs 23
you will need to get [package.el](http://bit.ly/pkg-el23) yourself if you wish to use it.

`haskell-mode` is available on both [Marmalade](http://marmalade-repo.org/packages/haskell-mode)
and [MELPA](http://melpa.milkbox.net) community maintained repos.

If you're not already using Marmalade, add this to your
`~/.emacs.d/init.el` and load it with `M-x eval-buffer`.

```lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

For MELPA the code you need to add is:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

And then you can install:

`M-x package-install [RET] haskell-mode [RET]`

or

```lisp
(when (not (package-installed-p 'haskell-mode))
  (package-install 'haskell-mode))
```

If the installation doesn't work right away try refreshing the package
list first:

`M-x package-refresh-contents [RET]`

### el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs.
If you're an el-get user just do `M-x el-get-install` to get `haskell-mode` installed.

### XEmacs

If you are using XEmacs, the haskell-mode package may be available for
installation through the XEmacs package UI.

### Emacs Prelude

`haskell-mode` is bundled with
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a
Prelude user you can start using it right away.

### Debian

If you are using Debian, you may be able to install the package
haskell-mode with a command like:

```bash
$ apt-get install haskell-mode
```

### Manual

-   Download and unpack the basic mode and modules into a suitable directory,
    e.g. ~/lib/emacs/haskell-mode/ where ~ stands for your home directory.

-   If you are using Emacs 21, you need an additional library, "syntax", from
    a later version of Emacs.  The one you can get as
    http://cvs.savannah.gnu.org/viewcvs/*checkout*/emacs/emacs/lisp/emacs-lisp/syntax.el?rev=1.16
    definitely works.

-   Assuming you have placed the basic mode haskell-mode.el and the modules
    you want to use in the directory ~/lib/emacs/haskell-mode/, add the
    following command to your init file (~/.emacs):
  
    ```lisp
    (load "~/lib/emacs/haskell-mode/haskell-site-file")
    ```

    This only loads the bare-bones haskell-mode. To make it useful, you
    need additional modules; you can use the haskell `customize-group`
    to edit the Haskell mode hook or, if you prefer manual setup, try
    adding the following lines according to which modules you want to use:

    ```lisp
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
    ```

    Note that the three indentation modules are mutually exclusive - add at
    most one.  Note that the line of code for simple indentation is commented
    out (using a preceeding `;`) in preference for the more advanced
    indentation module.  Installation is now complete!

The other modules are automatically loaded when needed in the following way:

-   Font locking: just turn it on via `global-font-lock-mode` or do
    `(add-hook 'haskell-mode-hook 'font-lock-mode)`

-   Declaration scanning: just use M-x imenu or bind `imenu` to a key.  E.g.
    `(global-set-key [(control meta down-mouse-3)] 'imenu)` or you can also add
    it to the menubar with `(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)`

-   Interaction with inferior Haskell interpreter: just hit `C-c C-z`  or  `C-c C-l`.


Setup
-----

Normally, inf-haskell automatically finds ghci or hugs in your PATH, but if
that's not the case (common under Windows), or if you need to specify your
preference, just tell Emacs which executable to use with:

```lisp
(setq haskell-program-name "/some/where/ghci.exe")
```

If you want to use different settings when you use Cygwin Emacs and NTEmacs,
you can test the value of `system-type`:

```lisp
(setq haskell-program-name
      (if (eq system-type 'cygwin)
          "/cygdrive/c/ghc/ghc-6.8.1/bin/ghcii.sh"
        "c:/ghc/ghc-6.8.1/bin/ghci.exe"))
```

Note that Cygwin binaries tend to interact poorly with NTEmacs, especially
w.r.t signal-handling.

Setup for new interactive mode
------------------------------

A new mode for the REPL and GHCi sessions is called
haskell-interactive-mode, it intends to replace inferior-haskell-mode,
but comes with different features.

There are new modules for handling the following things:

* Separate sessions per Cabal project (haskell-session.el).
* A new inferior Haskell process handling code (haskell-process.el).
* New REPL (haskell-interactive-mode.el).
* Bunch of new features based upon the above three things.

To make use of them, try out the instructions in
`examples/init.el`. WARNING: The features expressed in here are new
and many are Linux-specific.

Note: These features are entirely orthogonal to the older
inferior-haskell-mode, and therefore keybindings which work for
inferior-haskell-mode will not magically work for the above new
modules.


Customization
-------------

Most customizations are on the functionality of a particular module.
See the documentation of that module for information on its
customisation.

There is also a [wiki page listing tips and
tricks](http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs).

Known problems
--------------

It seems that some versions of XEmacs come without the fsf-compat package
(which provides functions such as `line-end-position`) and it seems that
even if your XEmacs does have the fsf-compat package installed it does not
autoload its part.  Thus you may have to install the fsf-compat package and
add `(require 'goto-addr)` in your .emacs.


Multi-mode editing
------------------

For LaTeX-based literate Haskell, you might be interested in the
multiple major mode package haskell-latex.el (plus multi-mode.el) from
http://www.loveshack.ukfsn.org/emacs/.


Support
-------

- [Github homepage](https://github.com/haskell/haskell-mode)
- [Mailing list](http://projects.haskell.org/cgi-bin/mailman/listinfo/haskellmode-emacs)

Contributing
------------

For submitting pull requests, please see the wiki
[page on contributing](https://github.com/haskell/haskell-mode/wiki/Contributing). You
don't have to follow this guide, but please make sure your pull
requests are at least properly rebased and up to date.
