# Development Instructions

The vscode directory is structured as follows:

    # Documentation
    README.md
    INSTALL.md
    CHANGELOG.md
    CONTRIBUTING.md

    # Extension Sources
    package.json
    extension/*.ts

    # Why3 Language
    why3.syntax.json    # syntax highlighting
    why3.language.json  # comments & brackets

    # Configuration
    VERSION          # generated
    dune             # VERSION generator
    .vscodeignore    # Packaged files
    tsconfig.json    # TypeScript config

    # Compilation
    Makefile

    # Compilation Internals
    yarn.lock
    node_modules/*
    out/*

    # Installable VSCode Extension
    why3-platform-<VERSION>.vsix

# Makefile Entries

All development tasks are accessible through the Makefile.
The main entries are:

    $ make build     # compile the sources, default
    $ make version   # update the version into package.json
    $ make lint      # typechecking the tscode
    $ make start     # Continuous building for debugging
    $ make build     # Compile statically to out (default target for make)
    $ make package   # Build & generate the vsix extension
    $ make clean     # Remove all compilation internals
    $ make install   # Install the generated extension (if any)
    $ make uninstall # Remove any installed why3find VSCode extension

# Debugging the Extension under VSCode

The VSCode extension debugger can be started with F5
It is recommended to use `make start` during debugging, although `make build`
also works.

# Notes

The version number is taken from why3find ocaml development which is assumed to
be up-to-date.

Headers must be updated from the Why3find root directory.
Use `make headers` from there.
