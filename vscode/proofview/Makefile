# --------------------------------------------------------------------------
# -- VS Code Proof View
# --------------------------------------------------------------------------

.PHONY: all start lint

all: build

start:
	yarn start

lint:
	yarn typecheck

build:
	yarn install
	yarn typecheck
	yarn build

package:
	yarn bundle
