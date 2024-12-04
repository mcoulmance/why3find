# --------------------------------------------------------------------------
# ---  Why3 Package Manager
# --------------------------------------------------------------------------

.PHONY: all build clean test lint headers install uninstall test-coverage

all: build

clean:
	dune clean

build:
	dune build

install:
	dune install 2> /dev/null

uninstall:
	dune uninstall 2> /dev/null

# --------------------------------------------------------------------------
# --- Linter
# --------------------------------------------------------------------------

SOURCES_MI=$(shell find src lib utils -name "*.ml" -or -name "*.mli")
SOURCES_ML=$(shell find src lib utils -name "*.ml*")
SOURCES_TS=$(shell find vscode -not -path "*/node_modules/*" -name "*.ts*")

lint:
	@ocp-indent -i $(SOURCES_MI)

headers:
	@headache -c headers.cfg -h HEADER $(SOURCES_ML) $(SOURCES_TS)

# --------------------------------------------------------------------------
# --- Tests
# --------------------------------------------------------------------------

test:
	@rm -f why3find.json
	dune test

test-coverage:
	@rm -rf _bisect
	@mkdir _bisect
	BISECT_FILE=$(shell pwd)/_bisect/bisect dune test --force --instrument-with bisect_ppx
	@bisect-ppx-report summary --coverage-path=_bisect
	@bisect-ppx-report html --coverage-path=_bisect
	@echo "Report: file://$(PWD)/_coverage/index.html"

# --------------------------------------------------------------------------
# --- VS Code Integration
# --------------------------------------------------------------------------

.PHONY: install-vscode

install-vscode:
	$(MAKE)
	$(MAKE) install
	$(MAKE) -C vscode package
	$(MAKE) -C vscode install

# --------------------------------------------------------------------------
