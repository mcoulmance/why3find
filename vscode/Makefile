# --------------------------------------------------------------------------
# -- VS Code Extension
# --------------------------------------------------------------------------

.PHONY: all
all: build

# --------------------------------------------------------------------------
# --- Versionning
# --------------------------------------------------------------------------

.PHONY: version

YARN=yarn --ignore-engines
VERSION=$(shell cat VERSION)

version: .version
.version: VERSION
	@$(YARN) version --no-git-tag-version --new-version $(VERSION)
	@touch .version

# --------------------------------------------------------------------------
# --- Linting
# --------------------------------------------------------------------------

.PHONY: lint
lint:
	$(YARN) typecheck
	$(YARN) --cwd proofview typecheck

# --------------------------------------------------------------------------
# --- Debugging
# --------------------------------------------------------------------------

.PHONY: start
start:
	@echo "------------------------------------------"
	@echo "---- Running Servers"
	@echo "------------------------------------------"
	@($(YARN) start &)
	@($(YARN) --cwd proofview start &)
	@echo "------------------------------------------"
	@echo "Servers Running… (Hit f5)"

# --------------------------------------------------------------------------
# --- Building
# --------------------------------------------------------------------------

.PHONY: build-extension
build-extension:
	@echo "------------------------------------------"
	@echo "---- Building Extension"
	@echo "------------------------------------------"
	$(YARN) install
	$(YARN) typecheck
	$(YARN) build

.PHONY: build-extension
build-view:
	@echo "------------------------------------------"
	@echo "---- Building Proof View"
	@echo "------------------------------------------"
	$(YARN) --cwd proofview install
	$(YARN) --cwd proofview typecheck
	$(YARN) --cwd proofview build

.PHONY: build package

build: build-extension build-view
	@echo "------------------------------------------"

package: build view
	@echo "------------------------------------------"
	@echo "---- Packaging VSCode"
	@echo "------------------------------------------"
	@rm -f *.vsix
	$(YARN) vsce ls
	$(YARN) package
	@echo "------------------------------------------"

# --------------------------------------------------------------------------
# -- Installing & Cleaning
# --------------------------------------------------------------------------

.PHONY: clean install uninstall

clean:
	$(MAKE) -C proofview clean
	rm -fr out node_modules *.vsix .version

install:
	code --install-extension why3-platform-*.vsix

uninstall:
	code --uninstall-extension undefined_publisher.why3-platform

# --------------------------------------------------------------------------
