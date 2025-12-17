# Why3 Platform

Provides support for the Why3 language and Why3find proof environment.

## Language Features

 - Syntax highlighting
 - Type-checking (after saving)
 - Goto definition (including doc refs)
 - Goto type definition (including doc refs)
 - Hover (full name, signature, cloned symbols)
 - Code Lens (proof feedback, dynamically updated)
 - Document Structure (namespaces, declarations)
 - Folding Ranges (proofs, sections)

## Debugger Features

 - Pause and Resume proof tasks
 - Step/in and Step/out through proof tree
 - Failed proofs diagnostic
 - Proof tree exploration through threads and stack variables
 - Proof context highlighting
 - Proof task preview

## Known Issues

Hovers are only available for top-level declarations, and only after
complete file type-checking. During code edition, code information is only
available _outside_ the modified area, until the file is saved again.
