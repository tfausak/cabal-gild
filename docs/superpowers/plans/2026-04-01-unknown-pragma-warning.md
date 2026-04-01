# Unknown Pragma Warning Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Emit a warning to stderr when a comment uses the `cabal-gild:` prefix but contains an unrecognized pragma name.

**Architecture:** Add a `MonadWarn` typeclass for stderr output, a `WarnUnknown` action module that detects unknown pragmas, and wire them into the existing pipeline. The `WarnUnknown` module checks all comments for the `cabal-gild:` prefix and warns if the body doesn't start with a known pragma name.

**Tech Stack:** Haskell, Cabal, Distribution.Parsec, hspec

---

### Task 1: Create `MonadWarn` class

**Files:**
- Create: `source/library/CabalGild/Unstable/Class/MonadWarn.hs`
- Modify: `cabal-gild.cabal` (add module to exposed-modules)

- [ ] **Step 1: Create the MonadWarn module**

```haskell
module CabalGild.Unstable.Class.MonadWarn where

import qualified System.IO as IO

-- | A 'Monad' that can also emit warnings.
class (Monad m) => MonadWarn m where
  -- | Emits the given warning message followed by a newline.
  warnLn :: String -> m ()

-- | Uses 'hPutStrLn' on 'stderr'.
instance MonadWarn IO where
  warnLn = IO.hPutStrLn IO.stderr
```

- [ ] **Step 2: Add module to cabal file**

In `cabal-gild.cabal`, in the `exposed-modules` list, add `CabalGild.Unstable.Class.MonadWarn` after `CabalGild.Unstable.Class.MonadLog` (alphabetical order).

- [ ] **Step 3: Verify it compiles**

Run: `cabal build`
Expected: successful build

- [ ] **Step 4: Commit**

```bash
git add source/library/CabalGild/Unstable/Class/MonadWarn.hs cabal-gild.cabal
git commit -m "Add MonadWarn class for emitting warnings to stderr"
```

---

### Task 2: Add `MonadWarn` instance to test monad

**Files:**
- Modify: `source/test-suite/Main.hs`

- [ ] **Step 1: Add import for MonadWarn**

After the existing `MonadLog` import (line 5), add:

```haskell
import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
```

- [ ] **Step 2: Add MonadWarn instance for TestT**

After the `MonadLog` instance (after line 2182), add the `MonadWarn` instance using the same writer mechanism:

```haskell
instance (Monad m) => MonadWarn.MonadWarn (TestT m) where
  warnLn = TestT . Trans.lift . RWST.tell . pure
```

This writes warnings into the same `W = [String]` writer as `MonadLog`, so existing test helpers that check `w` will capture both logs and warnings.

- [ ] **Step 3: Verify it compiles**

Run: `cabal build cabal-gild-test-suite`
Expected: successful build

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Main.hs
git commit -m "Add MonadWarn instance for test monad"
```

---

### Task 3: Create `WarnUnknown` action module

**Files:**
- Create: `source/library/CabalGild/Unstable/Action/EvaluatePragmas/WarnUnknown.hs`
- Modify: `cabal-gild.cabal` (add module to exposed-modules)

- [ ] **Step 1: Write the failing test**

In `source/test-suite/Main.hs`, change the existing "ignores unknown pragma" test (around line 1408) to assert that a warning IS emitted:

```haskell
  Hspec.it "warns on unknown pragma" $ do
    let (a, s, w) = runGild [] [(Input.Stdin, String.toUtf8 "-- cabal-gild: unknown")] (".", []) False
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` ["warning: unknown pragma \"unknown\""]
    actual <- case Map.toList s of
      [(Output.Stdout, x)] -> pure x
      _ -> fail $ "impossible: " <> show s
    actual `Hspec.shouldBe` String.toUtf8 "-- cabal-gild: unknown\n"
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cabal test`
Expected: FAIL — the warning list `w` will be `[]` instead of the expected warning.

- [ ] **Step 3: Create the WarnUnknown module**

```haskell
module CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown where

import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
import qualified CabalGild.Unstable.Extra.FieldLine as FieldLine
import qualified CabalGild.Unstable.Extra.Name as Name
import qualified CabalGild.Unstable.Type.Comment as Comment
import qualified CabalGild.Unstable.Type.Comments as Comments
import qualified CabalGild.Unstable.Type.Pragma as Pragma
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Distribution.Compat.CharParsing as CharParsing
import qualified Distribution.Fields as Fields
import qualified Distribution.Parsec as Parsec

run ::
  (MonadWarn.MonadWarn m) =>
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  m ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run (fs, cs) = do
  mapM_ field fs
  mapM_ warnComment cs
  pure (fs, cs)

field ::
  (MonadWarn.MonadWarn m) =>
  Fields.Field (p, Comments.Comments q) ->
  m ()
field f = case f of
  Fields.Field n fls -> do
    warnComments . snd $ Name.annotation n
    mapM_ (warnComments . snd . FieldLine.annotation) fls
  Fields.Section n _ fs -> do
    warnComments . snd $ Name.annotation n
    mapM_ field fs

warnComments ::
  (MonadWarn.MonadWarn m) =>
  Comments.Comments q ->
  m ()
warnComments cs = do
  mapM_ warnComment (Comments.before cs)
  mapM_ warnComment (Comments.after cs)

warnComment ::
  (MonadWarn.MonadWarn m) =>
  Comment.Comment q ->
  m ()
warnComment c = case Parsec.simpleParsecBS $ Comment.value c of
  Just (Pragma.Pragma (PragmaName name)) ->
    Monad.unless (name `elem` knownPragmaNames) $
      MonadWarn.warnLn $ "warning: unknown pragma \"" <> name <> "\""
  Nothing -> pure ()

-- | A type that parses just the pragma name (the first word after "cabal-gild:").
newtype PragmaName = PragmaName String

instance Parsec.Parsec PragmaName where
  parsec = do
    cs <- CharParsing.some (CharParsing.satisfy $ \c -> c /= ' ' && c /= '\t' && c /= '\n')
    -- Consume any remaining characters without failing
    Monad.void $ CharParsing.many CharParsing.anyChar
    pure $ PragmaName cs

-- | The set of known pragma names.
knownPragmaNames :: [String]
knownPragmaNames = ["discover", "require", "version"]
```

Note: `PragmaName` is wrapped in `Pragma` which handles the `cabal-gild:` prefix and leading whitespace. The `PragmaName` parser just captures the first word. We consume remaining characters so that `CharParsing.eof` in the `Pragma` parser succeeds. This means any comment with the prefix `-- cabal-gild: X` where X is a word will parse — we then check if X is in `knownPragmaNames`.

- [ ] **Step 4: Add module to cabal file**

In `cabal-gild.cabal`, in the `exposed-modules` list, add `CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown` after `CabalGild.Unstable.Action.EvaluatePragmas.Version` (alphabetical order).

- [ ] **Step 5: Wire into EvaluatePragmas.run**

Modify `source/library/CabalGild/Unstable/Action/EvaluatePragmas.hs`:

Add import:
```haskell
import qualified CabalGild.Unstable.Action.EvaluatePragmas.WarnUnknown as WarnUnknown
import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
```

Update the `run` function signature and body:
```haskell
run ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m, MonadWarn.MonadWarn m) =>
  FilePath ->
  ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q]) ->
  m ([Fields.Field (p, Comments.Comments q)], [Comment.Comment q])
run p =
  WarnUnknown.run
    Monad.>=> EvaluateRequire.run
    Monad.>=> EvaluateVersion.run
    Monad.>=> EvaluateDiscover.run p
```

- [ ] **Step 6: Add MonadWarn constraint to Main.hs**

In `source/library/CabalGild/Unstable/Main.hs`:

Add import:
```haskell
import qualified CabalGild.Unstable.Class.MonadWarn as MonadWarn
```

Add `MonadWarn.MonadWarn m` to the constraints of `mainWith`:
```haskell
mainWith ::
  ( MonadHandle.MonadHandle m,
    MonadLog.MonadLog m,
    MonadRead.MonadRead m,
    Exception.MonadThrow m,
    MonadWalk.MonadWalk m,
    MonadWarn.MonadWarn m,
    MonadWrite.MonadWrite m
  ) =>
  [String] ->
  m ()
```

Add `MonadWarn.MonadWarn m` to the constraints of `format`:
```haskell
format ::
  (Exception.MonadThrow m, MonadWalk.MonadWalk m, MonadWarn.MonadWarn m) =>
  FilePath ->
  ByteString.ByteString ->
  m ByteString.ByteString
```

- [ ] **Step 7: Run the test to verify it passes**

Run: `cabal test`
Expected: PASS — the "warns on unknown pragma" test should now see the warning in the writer output.

- [ ] **Step 8: Commit**

```bash
git add source/library/CabalGild/Unstable/Action/EvaluatePragmas/WarnUnknown.hs \
  source/library/CabalGild/Unstable/Action/EvaluatePragmas.hs \
  source/library/CabalGild/Unstable/Main.hs \
  source/test-suite/Main.hs \
  cabal-gild.cabal
git commit -m "Warn on unknown pragmas (closes #105)"
```

---

### Task 4: Add additional test coverage

**Files:**
- Modify: `source/test-suite/Main.hs`

- [ ] **Step 1: Add test that valid pragmas produce no warnings**

Near the new "warns on unknown pragma" test, add:

```haskell
  Hspec.it "does not warn on valid pragmas" $ do
    let (a, _s, w) = runGild [] [(Input.Stdin, String.toUtf8 "-- cabal-gild: version\nname: p")] (".", []) False
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
```

- [ ] **Step 2: Add test that regular comments produce no warnings**

```haskell
  Hspec.it "does not warn on regular comments" $ do
    let (a, _s, w) = runGild [] [(Input.Stdin, String.toUtf8 "-- just a comment\nname: p")] (".", []) False
    a `Hspec.shouldSatisfy` Either.isRight
    w `Hspec.shouldBe` []
```

- [ ] **Step 3: Run all tests**

Run: `cabal test`
Expected: all tests pass

- [ ] **Step 4: Commit**

```bash
git add source/test-suite/Main.hs
git commit -m "Add tests for pragma warning edge cases"
```
