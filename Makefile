VERSION = $(shell git describe --tags --abbrev=0 | sed 's/_/\./g')
GIT_VERSION = $(shell git describe --tags --dirty | sed 's/_/\./g')

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
DIST_FILES = $(ELFILES) $(ELCFILES) $(AUTOLOADS) logo.svg Makefile README.md NEWS
DIST_FILES_EX = examples/init.el examples/fontlock.hs examples/indent.hs
TGZ = haskell-mode-$(GIT_VERSION).tar.gz

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<

.PHONY: all compile info dist clean

all: compile $(AUTOLOADS)

compile: $(ELCFILES)

clean:
	$(RM) $(ELCFILES) $(AUTOLOADS) $(TGZ)

info: # No Texinfo file, sorry.

dist: $(TGZ)

$(AUTOLOADS): $(ELFILES) haskell-mode.elc
	[ -f $@ ] || echo '' >$@
	$(BATCH) --eval '(setq generated-autoload-file "'`pwd`'/$@")' -f batch-update-autoloads "."

# embed version number into .elc file
haskell-mode.elc: haskell-mode.el
	sed -e 's/@GIT_VERSION@/$(GIT_VERSION)/g;s/@VERSION@/$(VERSION)/g' < haskell-mode.el > haskell-mode.tmp.el #NO_DIST
	@$(BATCH) -f batch-byte-compile haskell-mode.tmp.el #NO_DIST
	mv haskell-mode.tmp.elc haskell-mode.elc #NO_DIST
	$(RM) haskell-mode.tmp.el #NO_DIST

$(TGZ): $(DIST_FILES)
	rm -rf haskell-mode-$(GIT_VERSION)
	mkdir haskell-mode-$(GIT_VERSION)
	cp -p $(DIST_FILES) haskell-mode-$(GIT_VERSION)
	mkdir haskell-mode-$(GIT_VERSION)/examples
	cp -p $(DIST_FILES_EX) haskell-mode-$(GIT_VERSION)/examples

	printf "1s/=.*/= $(VERSION)/\nw\n" | ed -s haskell-mode-$(GIT_VERSION)/Makefile #NO_DIST
	printf "2s/=.*/= $(GIT_VERSION)/\nw\n" | ed -s haskell-mode-$(GIT_VERSION)/Makefile #NO_DIST
	printf "g/NO_DIST/d\nw\n" | ed -s haskell-mode-$(GIT_VERSION)/Makefile #NO_DIST
	printf ',s/@VERSION@/$(VERSION)/\nw\n' | ed -s haskell-mode-$(GIT_VERSION)/haskell-mode.el #NO_DIST

	tar cvzf $(TGZ) haskell-mode-$(GIT_VERSION)
	rm -rf haskell-mode-$(GIT_VERSION)
