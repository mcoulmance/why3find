# --------------------------------------------------------------------------
# -- VS Code Extension
# --------------------------------------------------------------------------

.PHONY: all
all: build

# --- Versionning

.PHONY: version

YARN=yarn --ignore-engines
VERSION=$(shell cat VERSION)

version: .version
.version: VERSION
	@$(YARN) version --no-git-tag-version --new-version $(VERSION)
	@touch .version

# --- Linting & Building

.PHONY: lint build

lint:
	$(YARN) typecheck

build: version
	rm -f *.vsix
	rm -fr out
	$(YARN) install
	$(YARN) typecheck

build: lint
	$(YARN) bundle

package: build
	$(YARN) vsce ls
	$(MAKE) -C proofview package
	$(YARN) package

# -- Installing & Cleaning

.PHONY: clean install uninstall

clean:
	$(MAKE) -C proofview clean
	rm -fr out node_modules *.vsix .version

install: package
	code --install-extension why3-platform-*.vsix

uninstall:
	code --uninstall-extension undefined_publisher.why3-platform
