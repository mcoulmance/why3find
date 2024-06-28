# --------------------------------------------------------------------------
# -- VS Code Extension
# --------------------------------------------------------------------------

.PHONY: all build install

YARN=yarn --ignore-engines

all: build

watch:
	$(YARN) watch

build:
	rm -f *.vsix
	$(YARN) install
	$(YARN) build
	$(YARN) package

install:
	code --install-extension why3-platform-*.vsix
