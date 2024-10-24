# --------------------------------------------------------------------------
# -- VS Code Proof View
# --------------------------------------------------------------------------

.PHONY: all start lint

all: build

lint:
	yarn typecheck

watch:
	yarn watch

build:
	yarn install
	yarn typecheck
	yarn build
