# Why3 Platform

Provides support for the Why3 language and Why3find proof environment.

## Features

 - Syntax highlighting
 - Typechecking (update on save)
 - Goto definition
 - Goto type definition
 - Hover (full name, signature, updated on save)
 - Code Lens (proof feedback, dynamically updated)

## Known Issues

Hovers are only available for top-level declarations, and only after
complete file typechecking. During code edition, code information is only
available outside the modified area, until the file is saved again.
