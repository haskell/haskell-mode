VERSION = $(shell git describe --tags --dirty | sed 's/_/\./g')
EMACS = emacs
EFLAGS =
BATCH = $(EMACS) $(EFLAGS) --batch -Q -L .

ELFILES = \
	haskell-c.el \
	haskell-cabal.el \
	haskell-decl-scan.el \
	haskell-doc.el \
	haskell-font-lock.el \
	haskell-ghci.el \
	haskell-hugs.el \
	haskell-indent.el \
	haskell-indentation.el \
	haskell-checkers.el \
	haskell-mode.el \
	haskell-simple-indent.el \
	haskell-sort-imports.el \
	haskell-align-imports.el \
	haskell-move-nested.el \
	haskell-navigate-imports.el \
	haskell-interactive-mode.el \
	haskell-package.el \
	haskell-process.el \
	haskell-menu.el \
	haskell-session.el \
	haskell-string.el \
	haskell-show.el \
	ghc-core.el \
	inf-haskell.el

ELCFILES = $(ELFILES:.el=.elc)
# AUTOLOADS = $(PACKAGE)-startup.el
AUTOLOADS = haskell-site-file.el
DIST_FILES = $(ELFILES) $(ELCFILES) $(AUTOLOADS) logo.svg Makefile README.md
DIST_FILES_EX = examples/init.el examples/fontlock.hs examples/indent.hs
TGZ = haskell-mode-$(VERSION).tar.gz

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<

.PHONY: all compile info dist clean

all: compile $(AUTOLOADS)

compile: $(ELCFILES)

clean:
	$(RM) $(ELCFILES) $(AUTOLOADS) $(TGZ)

info: # No Texinfo file, sorry.

dist: $(TGZ)

$(AUTOLOADS): $(ELFILES)
	[ -f $@ ] || echo '' >$@
	$(BATCH) --eval '(setq generated-autoload-file "'`pwd`'/$@")' -f batch-update-autoloads "."

$(TGZ): $(DIST_FILES)
	rm -rf haskell-mode-$(VERSION)
	mkdir haskell-mode-$(VERSION)
	cp -p $(DIST_FILES) haskell-mode-$(VERSION)
	mkdir haskell-mode-$(VERSION)/examples
	cp -p $(DIST_FILES_EX) haskell-mode-$(VERSION)/examples
	tar cvzf $(TGZ) haskell-mode-$(VERSION)
	rm -rf haskell-mode-$(VERSION)
