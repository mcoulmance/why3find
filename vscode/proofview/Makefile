# --------------------------------------------------------------------------
# -- VS Code Proof View
# --------------------------------------------------------------------------

.PHONY: all start lint

all: build

start:
	yarn start

lint:
	yarn typecheck

build: lint
	yarn build

package:
	yarn install
	yarn typecheck
	yarn build
