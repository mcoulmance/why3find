# Changelog

## 1.3.0 - 2025-12-18

### Notable New Features

- Support for Debugger Adapter Protocol

### Command Line Interface

- New command `why3find dap [OPTIONS]`

### IDE Integration

- VSCode debugger client

### Bug Fixes

- Allow preprocessing tactics to leave the goal unchanged (#141, !187)

## 1.2.0 - 2025-08-01

### Command Line Interface

- New option `-X` to print failed tasks _in extenso_ (!165)
- New option `--show-progress=(never|auto|always)` to control when progress is
  shown (#128, !170)

### Bug Fixes

- Truncated float printing introduced in (!81) caused imprecise calibration
  values on reruns which could lead to spurious cache misses. Cache entries
  are now printed with sufficient precision. (#125, !169)
- Fix a failure when saving a new session (#137, !174)
- Removed duplicated symbols from `val function` in generated doc and LSP server (!176)
- Fixed missing symbol from `let rec` in generated doc and LSP server (!179)
- Cleanup sessions on success (!173, !180)
- Remove old theories from proof.json files (#130, !184)

## 1.1.1 - 2025-01-10

### Bug Fixes

- Fix the proving strategy to try only the first applicable tactic as stated
  in the doc. It would previously "backtrack" to the following tactic under
  certain circumstances (#121, !164).
- Fix dev-repo URI in opam file (!163).

## 1.1.0 - 2024-12-18

### Notable New Features

- Make the timeout of the fast sequential proof step an independent `fast`
  parameter instead of using 1/5 * `time`. See option `--fast` and README.md
  (!144).
- Introduce option `--preprocess <tactic>` to run a tactic before starting the
  proof strategy (!142).
- Add a LSP server and VSCode extension for the WhyML language. See the
  corresponding section below for detailed features.
- Improve cache efficiency by hashing tasks earlier in the Why3 processing
  (#93, !91).

### Command Line Interface

- Renamed option `-m` into `--master` (#99, !127)
- Renamed option `-v` into `--velocity` (#99, !127)
- New option `-l` for detailed statistics (!83, !146)
- New option `-g <name>` to select goal(s) to be proven (!114)
- New option `-H n` to run the proof strategy up to step n (!118)
- New option `--log-prover-results` to dump JSON prover results (!92)
- New option `--list-provers` to list available provers (!96)
- New option `--list-tactics` to list available tactics (!96)
- New option `--strict` to pin provers to their current version (#90, !86)
- New option `--relax` to un-pin provers from their configured version
  (#90, !86)
- New option `--update` to update provers' pins to latest version (!143)
- New option `--why3-debug` to enable Why3 debug flags (#88, !80)
- New option `--why3-warn-off` to disable Why3 warnings (#88, !80)
- Removed option `--extra-config` (no-more supported feature) (!79, !123)
- `--detect` and `--default` options are now strict and pin the provers to
  their current version (!137)

### LSP and VSCode Extension

- Syntax highlighting
- Typechecking (after saving)
- Goto definition (including doc refs)
- Goto type definition (including doc refs)
- Hover (full name, signature, cloned symbols)
- Code Lens (proof feedback, dynamically updated)
- Document Structure (namespaces, declarations)
- Folding Ranges (proofs, sections)

### Bug Fixes

- Fix how hammer strategy parameters are used in update and replay modes
  (!82, !129, !150)
- Fix highlighting in context dump mode (-x) (#78, !149, !157)
- Fix Why3 session generation, correctly setting the proved flag (#111, !145)
- Fix documentation links to internal modules (#75, !130)
- Avoid redundant warnings (#98, !126)
- Keep stuck goals in proof.json (#102, !121)
- Keep partial proofs when interrupted (#12, !114)
- Fix documentation of nested clone instances (!102)
- Load prover drivers once (!91)
- Fix parsing of `-p`, `-P`, `-T` and `-D` command-line arguments (#89, !85)
- Round times to millisecond in proof.json files (#87, !81)
- Load Why3 plugins and accept all registered input formats (#84, #85, !77)

### API

- Expose some of the why3find internal API as a library (!87)

## 1.0 - 2024-05-15

- Initial release
