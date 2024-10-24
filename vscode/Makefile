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

.PHONY: download lint watch build view package

download:
	$(YARN) install

lint:
	$(YARN) typecheck

watch:
	$(YARN) watch

build: download version lint
	@echo "------------------------------------------"
	@echo "---- Building Extension"
	@echo "------------------------------------------"
	@rm -f *.vsix
	@rm -fr out
	$(YARN) bundle

view:
	@echo "------------------------------------------"
	@echo "---- Building Proof View"
	@echo "------------------------------------------"
	$(MAKE) -C proofview build

package: build view
	@echo "------------------------------------------"
	@echo "---- Packaging VSCode"
	@echo "------------------------------------------"
	$(YARN) vsce ls
	$(YARN) package

# -- Installing & Cleaning

.PHONY: clean install uninstall

clean:
	$(MAKE) -C proofview clean
	rm -fr out node_modules *.vsix .version

install:
	code --install-extension why3-platform-*.vsix

uninstall:
	code --uninstall-extension undefined_publisher.why3-platform
