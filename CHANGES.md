# Changelog

All notable changes to the Why3find Platform will be documented in this file.

## [Unreleased]

### Command Line Interface

- Renamed option `-m` into `--master` (#99, !127)
- Renamed option `-v` into `--velocity` (#99, !127)
- New option `-H n` to run the proof strategy up to step n (!118)
- New option `--log-prover-results` to dump JSON prover results (!92)
- New option `--list-provers` to list available provers (!96)
- New option `--list-tactics` to list available tactics (!96)
- New option `--strict` to pin provers to their current version (!90)
- New option `--relax` to un-pin provers from their configured version (!90)
- New option `--why3-debug` to enable Why3 debug flags (#88, !80)
- New option `--why3-warn-off` to disable Why3 warnings (#88, !80)
- Removed option `--extra-config` (no-more supported feature) (!79, !123)
- New command `why3find lsp` (setup a Language Server Protocol) (!89)
- Show maximal prover time and tactic depth after proving (!83)

### VSCode Integration

- Syntax highlighting
- Typechecking (after saving)
- Goto definition (including doc refs)
- Goto type definition (including doc refs)
- Hover (full name, signature, cloned symbols)
- Code Lens (proof feedback, dynamically updated)
- Document Structure (namespaces, declarations)
- Folding Ranges (proofs, sections)

### Bug Fixes

- Fix documentation links to internal modules (#75, !130)
- Avoid redundant warnings (#98, !126)
- Keep stuck goals in proof.json (#102, !121)
- Keep partial proofs when interrupted (#12, !114)
- Fix documentation of nested clone instances (!102)
- Improve cache efficiency by hashing tasks before preparation (#93, !91)
- Load prover drivers once (!91)
- Fix parsing of `-p`, `-P`, `-T` and `-D` command-line arguments (#89, !85)
- Round times to millisecond in proof.json files (#87, !81)
- Load Why3 plugins and accept all registered input formats (#84, #85, !77)

### Ongoing Work

- Preparing an OCaml API for Why3find (!87)

## 1.0 — 2024-05-15

- Initial release
