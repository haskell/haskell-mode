VERSION = $(shell git describe --tags --match 'v[0-9]*' --abbrev=0 | sed 's/^v//;s/\.0*/./g')
GIT_VERSION = $(shell git describe --tags --match 'v[0-9]*' --long --dirty | sed 's/^v//')

EMACS = emacs
EFLAGS =
BATCH = $(EMACS) $(EFLAGS) --batch -Q -L .

ELFILES = \
	ghc-core.el \
	haskell-align-imports.el \
	haskell-c.el \
	haskell-cabal.el \
	haskell-checkers.el \
	haskell-compat.el \
	haskell-decl-scan.el \
	haskell-doc.el \
	haskell-font-lock.el \
	haskell-indent.el \
	haskell-indentation.el \
	haskell-interactive-mode.el \
	haskell-menu.el \
	haskell-mode.el \
	haskell-move-nested.el \
	haskell-navigate-imports.el \
	haskell-package.el \
	haskell-process.el \
	haskell-session.el \
	haskell-show.el \
	haskell-simple-indent.el \
	haskell-sort-imports.el \
	haskell-string.el \
	haskell-unicode-input-method.el \
	haskell-yas.el \
	inf-haskell.el

ELCFILES = $(ELFILES:.el=.elc)
AUTOLOADS = haskell-mode-autoloads.el

PKG_DIST_FILES = $(ELFILES) logo.svg README
PKG_TAR = haskell-mode-$(VERSION).tar
ELCHECKS=$(addprefix check-, $(ELFILES:.el=))

%.elc: %.el
	@$(BATCH) \
	   --eval "(byte-compile-disable-warning 'cl-functions)" \
       -f batch-byte-compile $<

.PHONY: all compile info clean check $(ELCHECKS) elpa package

all: compile $(AUTOLOADS)

compile: $(ELCFILES)

$(ELCHECKS): check-%: %.el
	@$(BATCH) --eval '(when (check-declare-file "$*.el") (error "check-declare failed"))'
	@$(BATCH) \
	     --eval "(setq byte-compile-error-on-warn t)" \
	 	 --eval "(byte-compile-disable-warning 'cl-functions)" \
		 -f batch-byte-compile $*.el
	@$(RM) $*.elc
	@echo "--"

check: $(ELCHECKS)
	@echo "checks passed!"

clean:
	$(RM) $(ELCFILES) $(AUTOLOADS) $(AUTOLOADS:.el=.elc) $(PKG_TAR)

info: # No Texinfo file, sorry.

# Generate ELPA-compatible package
package: $(PKG_TAR)
elpa: $(PKG_TAR)

$(PKG_TAR): $(PKG_DIST_FILES) haskell-mode-pkg.el.in
	rm -rf haskell-mode-$(VERSION)
	mkdir haskell-mode-$(VERSION)
	cp $(PKG_DIST_FILES) haskell-mode-$(VERSION)/
	sed -e 's/@VERSION@/$(VERSION)/g' < haskell-mode-pkg.el.in > haskell-mode-$(VERSION)/haskell-mode-pkg.el
	sed -e 's/@GIT_VERSION@/$(GIT_VERSION)/g;s/@VERSION@/$(VERSION)/g' < haskell-mode.el > haskell-mode-$(VERSION)/haskell-mode.el
	tar cvf $@ haskell-mode-$(VERSION)
	rm -rf haskell-mode-$(VERSION)
	@echo
	@echo "Created ELPA compatible distribution package '$@' from $(GIT_VERSION)"

$(AUTOLOADS): $(ELFILES) haskell-mode.elc
	$(BATCH) \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq generated-autoload-file "$(CURDIR)/$@")' \
		-f batch-update-autoloads "."

# HACK: embed version number into .elc file
haskell-mode.elc: haskell-mode.el
	sed -e 's/@GIT_VERSION@/$(GIT_VERSION)/g;s/@VERSION@/$(VERSION)/g' < haskell-mode.el > haskell-mode.tmp.el
	@$(BATCH) --eval "(byte-compile-disable-warning 'cl-functions)" -f batch-byte-compile haskell-mode.tmp.el
	mv haskell-mode.tmp.elc haskell-mode.elc
	$(RM) haskell-mode.tmp.el
