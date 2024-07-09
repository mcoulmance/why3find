# --------------------------------------------------------------------------
# -- VS Code Extension
# --------------------------------------------------------------------------

.PHONY: all lint watch build clean install uninstall

YARN=yarn --ignore-engines

all: build

lint:
	$(YARN) typecheck

watch:
	$(YARN) watch

build:
	rm -f *.vsix
	rm -fr out
	$(YARN) install
	$(YARN) typecheck
	$(YARN) bundle
	$(YARN) package

clean:
	rm -fr out node_modules *.vsix

install:
	code --install-extension why3-platform-*.vsix

uninstall:
	code --uninstall-extension undefined_publisher.why3-platform
