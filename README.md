Haskell Mode for Emacs
----------------------

[![Build Status](https://travis-ci.org/haskell/haskell-mode.png?branch=master)](https://travis-ci.org/haskell/haskell-mode)

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

### `package.el`-based Installation

*This is the recommended way*

`package.el` is the new
[built-in package manager](http://www.emacswiki.org/emacs/ELPA#toc4)
included in Emacs 24.x. On Emacs 23.x you will need to download
[`package.el`](http://bit.ly/pkg-el23) yourself and place `package.el`
somewhere in your
[`load-path`](http://www.emacswiki.org/emacs/LoadPath).

#### Marmalade

**Stable releases** of `haskell-mode` are available on
[Marmalade](http://marmalade-repo.org/packages/haskell-mode).

If you're not already using Marmalade, add the following snippet to
your `.emacs` or `~/.emacs.d/init.el` and evaluate it with
`M-x eval-buffer`:

```el
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

Refresh the package index by `M-x package-refresh-contents` and install
`haskell-mode` via `M-x package-install [RET] haskell-mode`.

Alternatively, you can also download the `.tar` file via the
_Download_ link at http://marmalade-repo.org/packages/haskell-mode and
install the package `.tar`-file via `M-x package-install-file`

#### MELPA

**Unstable snapshots** can be installed via the
[MELPA](http://melpa.milkbox.net) community maintained repository.

For MELPA the code you need to add is:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

Refresh the package index by `M-x package-refresh-contents` and install
`haskell-mode` via `M-x package-install [RET] haskell-mode`.

### el-get based Installation

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

If you are using Debian, you can install an older version (e.g. Wheezy
ships with version 2.8.0) of `haskell-mode` with a command like:

```bash
$ apt-get install haskell-mode
```

### Manual

*This installation method is rather hacky and recommended for haskell-mode developers/contributors only as it allows to load haskell-mode directly from the git working copy. If you just want to use bleeding edge versions of haskell-mode please use the MELPA method described above.*

-   Download and unpack (for instance by by `git clone`) the basic mode and modules
    into a suitable directory, e.g. `~/lib/emacs/haskell-mode/` where `~` stands for
    your home directory.

-   If you are using Emacs 21, you need an additional library, "syntax", from
    a later version of Emacs.  The one you can get as
    http://cvs.savannah.gnu.org/viewcvs/*checkout*/emacs/emacs/lisp/emacs-lisp/syntax.el?rev=1.16
    definitely works.

-   Assuming you have placed the basic mode (`haskell-mode.el`) and the other modules
    you want to use in the directory ~/lib/emacs/haskell-mode/, you need generate the
    autoloads file (`haskell-site-file.el`) by either

    - Invoking `make all` or `make haskell-site-file.el`, or
    
    - From inside Emacs, `M-x update-directory-autoloads` and answering the question for
      the folder with `~/lib/emacs/haskell-mode/` and the question for the output-file with
      `~/lib/emacs/haskell-mode/haskell-site-file.el`
    
    and then adding the following command to your init file (`~/.emacs` or `~/.emacs.d/init.el`):
  
    ```el
    (add-to-list 'load-path "~/lib/emacs/haskell-mode/")
    (require 'haskell-mode-autoloads)
    ```

Basic Configuration
-------------------

In its default state, only the bare-bones haskell-mode is active.  To
make it useful, you need additional modules; you can use the haskell
`customize-group` to edit the Haskell mode hook or, if you prefer
manual setup, try adding the following lines according to which
modules you want to use:

```el
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
```

```el
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
```

Note that the three indentation modules are mutually exclusive - add at
most one.  Note that the line of code for simple indentation is commented
out (using a preceding `;`) in preference for the more advanced
indentation module.  Installation is now complete!

Other modules can be automatically loaded when needed in the following
way:

-   Font locking: just turn it on via `global-font-lock-mode` or do
    `(add-hook 'haskell-mode-hook 'font-lock-mode)`

-   Declaration scanning: just use `M-x imenu` or bind `imenu` to a key.  E.g.
    `(global-set-key [(control meta down-mouse-3)] 'imenu)` or you can also add
    it to the menubar with `(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)`

### Interactive block indentation (`haskell-move-nested.el`)

By inserting the key bindings for `C-,` and `C-.` (these bindings are
convenient on keyboard layouts where `,` and `.` are adjacent keys) as
shown below you can interactively de/indent either the following
nested block or, if a region is active while in transient-mark-mode,
de/indent the active region.

By using a numeric
[prefix argument](http://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html),
you can modify the shift-amount e.g. `C-u C-,` increases indentation
by 4 characters at once.

```el
(define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
(define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
```

### Setup for inferior haskell mode

This is the traditional mode for interacting with the Haskell interpreter.

By default the `haskell-mode-map` keymap is setup to use this mode, e.g.

-   `C-c C-z` is bound to `switch-to-haskell`
-   `C-c C-b` is bound to `switch-to-haskell`
-   `C-c C-l` is bound to `inferior-haskell-load-file`
-   `C-c C-t` is bound to `inferior-haskell-type`
-   `C-c C-i` is bound to `inferior-haskell-info`

Normally, inf-haskell automatically finds ghci or hugs in your PATH,
but if that's not the case (common under Windows), or if you need to
specify your preference, just tell Emacs which executable to use with:

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

Note that Cygwin binaries tend to interact poorly with NTEmacs,
especially w.r.t signal-handling.

### Setup for haskell interactive mode

A new mode for the REPL and GHCi sessions is called
haskell-interactive-mode, it intends to replace inferior-haskell-mode,
but comes with different features.

There are new modules for handling the following things:

* Separate sessions per Cabal project (`haskell-session.el`).
* A new inferior Haskell process handling code (`haskell-process.el`).
* New REPL (`haskell-interactive-mode.el`).
* Bunch of new features based upon the above three things.

To make use of them, try out the instructions in
`examples/init.el`. WARNING: The features expressed in here are new
and many are Linux-specific.

Note: These features are entirely orthogonal to the older
inferior-haskell-mode, and therefore key-bindings which work for
inferior-haskell-mode will not magically work for the above new
modules.

### Haskell Unicode input method

This package provides `haskell-unicode-input-method`, an input
method which allows you to easily type a number of Unicode symbols
that are useful when writing Haskell code.

To automatically load in haskell-mode put the following code in your
`.emacs` file:

```el
(require 'haskell-unicode-input-method)
(add-hook 'haskell-mode-hook
  (lambda () (set-input-method "haskell-unicode")))
```

Make sure the directory containing the .el file is in your load-path,
for example:

```el
(add-to-list 'load-path "~/.elisp/emacs-haskell-unicode-input-method")
```

To manually enable use `M-x set-input-method` or `C-x [RET] C-\` with
`haskell-unicode`. Note that the elisp file must be evaluated for this
to work.

Now you can simply type `->` and it is immediately replaced with
`→`. Use `C-\` to toggle the input method. To see a table of all key
sequences use `M-x describe-input-method haskell-unicode`. A sequence
like `<=` is ambiguous and can mean either `⇐` or `≤`. Typing it
presents you with a choice. Type `1` or `2` to select an option or
keep typing to use the default option.

If you don't like the highlighting of partially matching tokens you
can turn it off:

    (setq input-method-highlight-flag nil)

### Further Customization

Most customizations are on the functionality of a particular module.
See the documentation of that module for information on its
customization.

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
