# --------------------------------------------------------------------------
# -- VS Code Extension
# --------------------------------------------------------------------------

.PHONY: all build install

all: build

update:
	yarn --ignore-engines install

build:
	yarn --ignore-engines build

package:
	rm -f *.vsix
	vsce package --yarn

install:
	code --install-extension why3-platform-*.vsix
