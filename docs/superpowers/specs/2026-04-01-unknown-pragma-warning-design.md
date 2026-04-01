# Unknown Pragma Warning

Issue: https://github.com/tfausak/cabal-gild/issues/105

## Problem

When a comment starts with `-- cabal-gild:` but contains an unrecognized pragma name, cabal-gild silently ignores it. This makes it hard to diagnose typos or version mismatches (e.g., using a pragma from a newer version of cabal-gild).

## Solution

Emit a warning to stderr when an unknown pragma is encountered. No CLI flag is needed — warnings are always emitted.

## Warning Format

```
warning: unknown pragma "X"
```

Where `X` is the text after `cabal-gild:` that failed to match a known pragma.

## New Components

### `MonadWarn` class

**File:** `CabalGild.Unstable.Class.MonadWarn`

```haskell
class (Monad m) => MonadWarn m where
  warnLn :: String -> m ()
```

- **IO instance:** `hPutStrLn stderr`
- **TestT instance:** appends to the existing writer (`W = [String]`), same mechanism used by `MonadLog`

### `WarnUnknown` action

**File:** `CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown`

Walks all comments (top-level and attached to fields/sections via `Comments`). For each comment:

1. Attempt to parse the `cabal-gild:` prefix.
2. If the prefix matches, check whether the body starts with a known pragma name (`discover`, `version`, `require`).
3. If not, emit `warning: unknown pragma "X"` via `MonadWarn.warnLn`.

### Known pragma names

The set of known pragma names is: `discover`, `version`, `require`. This is defined in `WarnUnknown` as a list of strings.

## Integration

### `EvaluatePragmas.run`

Gains a `MonadWarn` constraint. Composes `WarnUnknown.run` as the first step:

```haskell
run p =
  WarnUnknown.run
    >=> EvaluateRequire.run
    >=> EvaluateVersion.run
    >=> EvaluateDiscover.run p
```

### `Main.hs`

`format` and `mainWith` gain the `MonadWarn` constraint. The IO instance routes warnings to stderr.

## What Does NOT Trigger a Warning

- Comments that don't start with `cabal-gild:` (regular comments).
- Valid pragmas (`discover`, `version`, `require`) even if they are in an invalid position (e.g., `discover` outside a relevant field).
- Near-misses of the prefix (e.g., `-- cabal-gild : discover` with extra space before colon).

## Testing

- Update the existing "ignores unknown pragma" test to assert that a warning string is present in the writer output.
- Add a test confirming that valid pragmas do not produce warnings.
- Add a test confirming that regular comments (without the `cabal-gild:` prefix) do not produce warnings.
