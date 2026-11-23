# AGENTS.md

## Haskell conventions

This is a Cabal project. Follow the existing project structure and style.

- Use `cabal build`, `cabal test`, and `cabal repl` for building, testing,
  and interactive exploration.
- Run project executables through `cabal exec <name> -- ...`; do not invoke
  binaries from `dist-newstyle` directly.
- Prefer targeted builds/tests during iteration, then run the relevant full
  checks before finishing.
- Treat `dist-newstyle/`, `*.cabal` and other generated directories as opaque; do not
  search, inspect, or edit them.
- Make small, reviewable changes. Do not refactor unrelated code.

### Code style

- Prefer `newtype` over a single-field `data` declaration when their
  semantics are appropriate.
- Prefer `Text` over `String` for application data. Convert at external API
  boundaries when necessary.
- Prefer total functions and exhaustive pattern matching.
- Avoid `undefined`, `error`, `fromJust`, `head`, and similar partial
  functions unless an existing invariant clearly justifies them.
- Prefer explicit, readable code over clever abstractions.
- Avoid complicated point-free expressions; name intermediate operations
  when that improves readability.
- Do not introduce unnecessary type classes, transformer layers, or
  type-level machinery.
- Follow existing conventions for records, strictness, effects, and error
  handling.

### Language extensions

Before adding a `{-# LANGUAGE ... #-}` pragma to a source file, inspect
`*.cabal` for project/component-wide `default-extensions`.

- Do not duplicate an extension already enabled by `*.cabal`.
- Prefer the existing project-wide extension policy.
- Add a file-local extension only when it is genuinely local to that module.
- Do not add extensions merely to work around a type/design problem.

### Exploration

- Use `rg` for textual search and `fd` for files.
- Use `cabal repl` to inspect types and test small expressions when useful.
- Do not recursively search `dist-newstyle/`, `.git/`, or generated output.
- When uncertain about a Haskell type, instance, or imported API, prefer
  checking it with `cabal repl` (`:t`, `:i`, `:kind!`) rather than guessing.

### Before finishing

- Inspect the diff for accidental or unrelated changes.
- Use `ormolu` and `cabal-gild` for formatting.
- Run the relevant linter if the project already uses one.
- Run `cabal build` and the relevant tests.
- Do not suppress warnings or change expected test results merely to make
  checks pass.
