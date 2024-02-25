{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified CabalGild.Class.MonadLog as MonadLog
import qualified CabalGild.Class.MonadRead as MonadRead
import qualified CabalGild.Class.MonadWalk as MonadWalk
import qualified CabalGild.Class.MonadWrite as MonadWrite
import qualified CabalGild.Extra.String as String
import qualified CabalGild.Main as Gild
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.RWS as RWST
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Function as Function
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map
import qualified GHC.Stack as Stack
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.parallel . Hspec.describe "cabal-gild" $ do
  Hspec.it "shows the help" $ do
    let (a, s, w) =
          runTest
            (Gild.mainWith "" ["--help"])
            (Map.empty, Map.empty)
            Map.empty
    a `Hspec.shouldSatisfy` Either.isLeft
    w `Hspec.shouldNotSatisfy` null
    s `Hspec.shouldBe` Map.empty

  Hspec.it "shows the version" $ do
    let (a, s, w) =
          runTest
            (Gild.mainWith "" ["--version"])
            (Map.empty, Map.empty)
            Map.empty
    a `Hspec.shouldSatisfy` Either.isLeft
    w `Hspec.shouldNotSatisfy` null
    s `Hspec.shouldBe` Map.empty

  Hspec.it "reads from an input file" $ do
    let (a, s, w) =
          runTest
            (Gild.mainWith "" ["--input", "input.cabal"])
            (Map.singleton (Just "input.cabal") (String.toUtf8 ""), Map.empty)
            Map.empty
    a `Hspec.shouldBe` Right ()
    w `Hspec.shouldBe` []
    s `Hspec.shouldSatisfy` Map.member Nothing

  Hspec.it "writes to an output file" $ do
    let (a, s, w) =
          runTest
            (Gild.mainWith "" ["--output", "output.cabal"])
            (Map.singleton Nothing (String.toUtf8 ""), Map.empty)
            Map.empty
    a `Hspec.shouldBe` Right ()
    w `Hspec.shouldBe` []
    s `Hspec.shouldSatisfy` Map.member (Just "output.cabal")

  Hspec.it "succeeds when checking formatted input" $ do
    let (a, s, w) =
          runTest
            (Gild.mainWith "" ["--mode", "check"])
            (Map.singleton Nothing (String.toUtf8 ""), Map.empty)
            Map.empty
    a `Hspec.shouldBe` Right ()
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "fails when checking unformatted input" $ do
    let (a, s, w) =
          runTest
            (Gild.mainWith "" ["--mode", "check"])
            (Map.singleton Nothing (String.toUtf8 "fail:yes"), Map.empty)
            Map.empty
    a `Hspec.shouldSatisfy` Either.isLeft
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "succeeds when checking CRLF input" $ do
    let (a, s, w) =
          runTest
            (Gild.mainWith "" ["--mode", "check"])
            (Map.singleton Nothing (String.toUtf8 "pass: yes\r\n"), Map.empty)
            Map.empty
    a `Hspec.shouldBe` Right ()
    w `Hspec.shouldBe` []
    s `Hspec.shouldBe` Map.empty

  Hspec.it "succeeds with empty input" $ do
    expectGilded
      ""
      ""

  Hspec.it "removes extra blank space" $ do
    expectGilded
      "\t\r\n \r\n"
      ""

  Hspec.it "formats a comment" $ do
    expectGilded
      "-- c"
      "-- c\n"

  Hspec.it "keeps a blank comment" $ do
    expectGilded
      "--"
      "--\n"

  Hspec.it "formats multiple comments" $ do
    expectGilded
      "-- c\n-- d"
      "-- c\n-- d\n"

  Hspec.it "removes blank lines between comments" $ do
    expectGilded
      "-- c\n\n-- d"
      "-- c\n-- d\n"

  Hspec.it "does not require a space after the comment start" $ do
    expectGilded
      "--c"
      "--c\n"

  Hspec.it "leaves leading blank space in comments" $ do
    expectGilded
      "--\t c"
      "--\t c\n"

  Hspec.it "trims trailing blank space from comments" $ do
    expectGilded
      "-- c\t \n"
      "-- c\n"

  Hspec.it "normalizes Windows line endings in comments" $ do
    expectGilded
      "-- c\r\n"
      "-- c\n"

  Hspec.it "formats a field without a value" $ do
    expectGilded
      "f:"
      "f:\n"

  Hspec.it "formats a field with a value" $ do
    expectGilded
      "f: 1"
      "f: 1\n"

  Hspec.it "does not require spaces before a field's value" $ do
    expectGilded
      "f:1"
      "f: 1\n"

  Hspec.it "allows spaces after a field's name" $ do
    expectGilded
      "f :1"
      "f: 1\n"

  Hspec.it "formats multiple fields" $ do
    expectGilded
      "f: 1\ng: 2"
      "f: 1\ng: 2\n"

  Hspec.it "formats a field with hung values" $ do
    expectGilded
      "f: 1\n 2"
      "f:\n  1\n  2\n"

  Hspec.it "formats a field with nested values" $ do
    expectGilded
      "f:\n 1\n 2"
      "f:\n  1\n  2\n"

  Hspec.it "puts a line after a field with multiple values" $ do
    expectGilded
      "f: 1\n 2\ng: 3"
      "f:\n  1\n  2\n\ng: 3\n"

  Hspec.it "does not pu a line before a field with multiple values" $ do
    expectGilded
      "f: 1\ng: 2\n 3"
      "f: 1\ng:\n  2\n  3\n"

  Hspec.it "converts field names to lowercase" $ do
    expectGilded
      "F: 1"
      "f: 1\n"

  Hspec.it "does not convert field values to lowercase" $ do
    expectGilded
      "f: A"
      "f: A\n"

  Hspec.it "normalizes spaces within field values" $ do
    expectGilded
      "f:\n a\n\n  b"
      "f:\n  a\n  b\n"

  Hspec.it "formats a section" $ do
    expectGilded
      "s"
      "s\n"

  Hspec.it "puts a line after a section" $ do
    expectGilded
      "s\nf: 1"
      "s\n\nf: 1\n"

  Hspec.it "puts a line before a section" $ do
    expectGilded
      "f: 1\ns"
      "f: 1\n\ns\n"

  Hspec.it "formats a section with an argument" $ do
    expectGilded
      "s a"
      "s a\n"

  Hspec.it "formats a section with multiple arguments" $ do
    expectGilded
      "s a b"
      "s a b\n"

  Hspec.it "formats a section with a quoted argument" $ do
    expectGilded
      "s \"a\""
      "s \"a\"\n"

  Hspec.it "formats a section with an operator argument" $ do
    expectGilded
      "s !"
      "s !\n"

  Hspec.it "converts section names to lowercase" $ do
    expectGilded
      "S"
      "s\n"

  Hspec.it "does not convert section arguments to lowercase" $ do
    expectGilded
      "s A"
      "s A\n"

  Hspec.it "formats nested sections" $ do
    -- Note that the nested section does not have an extra blank line before
    -- it.
    expectGilded
      "s\n t"
      "s\n  t\n"

  Hspec.it "formats a section with a field" $ do
    expectGilded
      "s\n f: 1"
      "s\n  f: 1\n"

  Hspec.it "formats a section with multiple fields" $ do
    expectGilded
      "s\n f: 1\n g: 2"
      "s\n  f: 1\n  g: 2\n"

  Hspec.it "correctly indents a section containing a field with multiple values" $ do
    expectGilded
      "s\n f:\n  1\n  2"
      "s\n  f:\n    1\n    2\n"

  Hspec.it "formats a comment before a field" $ do
    expectGilded
      "-- c\nf: 1"
      "-- c\nf: 1\n"

  Hspec.it "formats a comment after a field" $ do
    expectGilded
      "f: 1\n-- c"
      "f: 1\n-- c\n"

  Hspec.it "formats a comment before a field's value" $ do
    expectGilded
      "f:\n -- c\n 1"
      "f:\n  -- c\n  1\n"

  Hspec.it "formats a comment in a field's value" $ do
    expectGilded
      "f:\n 1\n -- c\n 2"
      "f:\n  1\n  -- c\n  2\n"

  Hspec.it "formats a comment after a field's value" $ do
    expectGilded
      "f:\n 1\n -- c"
      "f: 1\n-- c\n"

  Hspec.it "formats a comment after a field with multiple values" $ do
    expectGilded
      "f: 1\n 2\n-- c"
      "f:\n  1\n  2\n\n-- c\n"

  Hspec.it "formats a comment before a section" $ do
    expectGilded
      "-- c\ns"
      "-- c\ns\n"

  Hspec.it "formats a comment after a section" $ do
    expectGilded
      "s\n-- c"
      "s\n\n-- c\n"

  Hspec.it "correctly indents a comment in a section" $ do
    expectGilded
      "s\n -- c\n f: 1"
      "s\n  -- c\n  f: 1\n"

  Hspec.describe "description" $ do
    -- These tests apply to other "free text" fields as well. The description
    -- field is just a representative example.

    Hspec.describe "< 3.0" $ do
      -- These may look surprising, but they're the expected behavior.
      -- <https://github.com/haskell/cabal/issues/5938>

      Hspec.it "collapses extra blank lines" $ do
        expectGilded
          "description:\n 1\n\n 2"
          "description:\n  1\n  2\n"

      Hspec.it "trims leading blank space" $ do
        expectGilded
          "description:\n 1\n  2"
          "description:\n  1\n  2\n"

    Hspec.describe ">= 3.0" $ do
      Hspec.it "keeps one extra blank line" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n 1\n\n 2"
          "cabal-version: 3.0\ndescription:\n  1\n\n  2\n"

      Hspec.it "keeps two extra blank lines" $ do
        -- Although Haddock will collapse these, Cabal itself keeps them.
        expectGilded
          "cabal-version: 3.0\ndescription:\n 1\n\n\n 2"
          "cabal-version: 3.0\ndescription:\n  1\n\n\n  2\n"

      Hspec.it "indents second line past first when nested" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n 1\n  2"
          "cabal-version: 3.0\ndescription:\n  1\n   2\n"

      Hspec.it "indents first line past second when nested" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n  1\n 2"
          "cabal-version: 3.0\ndescription:\n   1\n  2\n"

      Hspec.it "does not indent first line when hung" $ do
        expectGilded
          "cabal-version: 3.0\ndescription: 1\n 2"
          "cabal-version: 3.0\ndescription:\n  1\n  2\n"

      Hspec.it "indents second line past first when hung" $ do
        expectGilded
          "cabal-version: 3.0\ndescription: 1\n              2"
          "cabal-version: 3.0\ndescription:\n  1\n   2\n"

      Hspec.it "indents third line (but not first) past second when hung" $ do
        -- If the first line of the value is on the same line as the name, then
        -- it should never be indented.
        expectGilded
          "cabal-version: 3.0\ndescription: 1\n 2\n  3"
          "cabal-version: 3.0\ndescription:\n  1\n  2\n   3\n"

      Hspec.it "treats tabs the same as spaces" $ do
        -- Yes, each tab only counts as one space.
        expectGilded
          "cabal-version: 3.0\ndescription:\n 1\n\t2"
          "cabal-version: 3.0\ndescription:\n  1\n  2\n"

      Hspec.it "does not insert extra blank lines before comments" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n -- c\n 1\n -- d\n 2"
          "cabal-version: 3.0\ndescription:\n  -- c\n  1\n  -- d\n  2\n"

      Hspec.it "does not consider comments for indentation" $ do
        expectGilded
          "cabal-version: 3.0\ndescription:\n  1\n -- c\n    2"
          "cabal-version: 3.0\ndescription:\n  1\n  -- c\n    2\n"

  Hspec.it "properly formats conditionals" $ do
    expectGilded
      "if ! impl ( ghc >= 9.8 )"
      "if !impl(ghc >= 9.8)\n"

  Hspec.describe "license-files" $ do
    -- These tests apply to other "list" fields as well. The license-files
    -- field is just a representative example.

    Hspec.it "removes duplicates" $ do
      expectGilded
        "license-files: f f"
        "license-files: f\n"

    Hspec.it "sorts unquoted" $ do
      expectGilded
        "license-files: g f"
        "license-files:\n  f\n  g\n"

    Hspec.it "sorts mixed" $ do
      expectGilded
        "license-files: \"g\" f"
        "license-files:\n  f\n  g\n"

    Hspec.it "sorts quoted" $ do
      expectGilded
        "license-files: \"g\" \"f\""
        "license-files:\n  f\n  g\n"

    Hspec.it "sorts comma-separated" $ do
      expectGilded
        "license-files: g, f"
        "license-files:\n  f\n  g\n"

    Hspec.it "collects comments at the top" $ do
      -- This isn't ideal, but keeping comments "close" to their original
      -- identifier is very difficult.
      expectGilded
        "license-files:\n -- c\n g\n -- d\n f"
        "license-files:\n  -- c\n  -- d\n  f\n  g\n"

    Hspec.it "leaves invalid fields alone" $ do
      -- This field value is valid in general. Cabal will reject it because the
      -- license-files field has special rules. Perhaps the user is trying to
      -- format something other than a package description. Or maybe they just
      -- made a typo. Either way, we can leave it alone.
      expectGilded
        "license-files: , f ,"
        "license-files: , f ,\n"

  Hspec.it "sorts tested-with" $ do
    expectGilded
      "tested-with: GHC == 9.8.1 , GHC == 9.6.4"
      "tested-with:\n  ghc ==9.6.4\n  ghc ==9.8.1\n"

  Hspec.it "sorts data-files" $ do
    expectGilded
      "data-files: g f"
      "data-files:\n  f\n  g\n"

  Hspec.it "sorts extra-source-files" $ do
    expectGilded
      "extra-source-files: g f"
      "extra-source-files:\n  f\n  g\n"

  Hspec.it "sorts extra-tmp-files" $ do
    expectGilded
      "extra-tmp-files: g f"
      "extra-tmp-files:\n  f\n  g\n"

  Hspec.it "sorts extra-doc-files" $ do
    expectGilded
      "extra-doc-files: g f"
      "extra-doc-files:\n  f\n  g\n"

  Hspec.it "sorts exposed-modules" $ do
    expectGilded
      "library\n exposed-modules: N M"
      "library\n  exposed-modules:\n    M\n    N\n"

  Hspec.it "sorts reexported-modules" $ do
    expectGilded
      "library\n reexported-modules: p:M4 as M5, M2 as M3, q:M6, M1"
      "library\n  reexported-modules:\n    M1,\n    M2 as M3,\n    p:M4 as M5,\n    q:M6\n"

  Hspec.it "sorts signatures" $ do
    expectGilded
      "library\n signatures: N M"
      "library\n  signatures:\n    M\n    N\n"

  Hspec.it "sorts build-tools" $ do
    expectGilded
      "library\n build-tools: q , p == 1"
      "library\n  build-tools:\n    p ==1,\n    q >=0\n"

  Hspec.it "adds a trailing comma to build-tools when possible" $ do
    expectGilded
      "cabal-version: 2.2\nlibrary\n build-tools: p == 1, q == 2"
      "cabal-version: 2.2\n\nlibrary\n  build-tools:\n    p ==1,\n    q ==2,\n"

  Hspec.it "does not add a trailing comma to build-tools with one element" $ do
    expectGilded
      "cabal-version: 2.2\nlibrary\n build-tools: p"
      "cabal-version: 2.2\n\nlibrary\n  build-tools: p >=0\n"

  Hspec.it "sorts build-tool-depends" $ do
    expectGilded
      "library\n build-tool-depends: q:d , p:c == 1"
      "library\n  build-tool-depends:\n    p:c ==1,\n    q:d\n"

  Hspec.it "sorts pkgconfig-depends" $ do
    -- This unusual version range format appears to be a quirk of Cabal's
    -- parser specifically for pkg-config.
    expectGilded
      "library\n pkgconfig-depends: q , p == 1"
      "library\n  pkgconfig-depends:\n    p >=1 && <=1,\n    q\n"

  Hspec.it "sorts frameworks" $ do
    expectGilded
      "library\n frameworks: b a"
      "library\n  frameworks:\n    a\n    b\n"

  Hspec.it "sorts extra-framework-dirs" $ do
    expectGilded
      "library\n extra-framework-dirs: e d"
      "library\n  extra-framework-dirs:\n    d\n    e\n"

  Hspec.it "sorts asm-sources" $ do
    expectGilded
      "library\n asm-sources: g f"
      "library\n  asm-sources:\n    f\n    g\n"

  Hspec.it "sorts cmm-sources" $ do
    expectGilded
      "library\n cmm-sources: g f"
      "library\n  cmm-sources:\n    f\n    g\n"

  Hspec.it "sorts c-sources" $ do
    expectGilded
      "library\n c-sources: g f"
      "library\n  c-sources:\n    f\n    g\n"

  Hspec.it "sorts cxx-sources" $ do
    expectGilded
      "library\n cxx-sources: g f"
      "library\n  cxx-sources:\n    f\n    g\n"

  Hspec.it "sorts js-sources" $ do
    expectGilded
      "library\n js-sources: g f"
      "library\n  js-sources:\n    f\n    g\n"

  Hspec.it "sorts other-modules" $ do
    expectGilded
      "library\n other-modules: N M"
      "library\n  other-modules:\n    M\n    N\n"

  Hspec.it "sorts virtual-modules" $ do
    expectGilded
      "library\n virtual-modules: N M"
      "library\n  virtual-modules:\n    M\n    N\n"

  Hspec.it "sorts autogen-modules" $ do
    expectGilded
      "library\n autogen-modules: N M"
      "library\n  autogen-modules:\n    M\n    N\n"

  Hspec.it "sorts other-languages" $ do
    expectGilded
      "library\n other-languages: b a"
      "library\n  other-languages:\n    a\n    b\n"

  Hspec.it "sorts default-extensions" $ do
    expectGilded
      "library\n default-extensions: b a"
      "library\n  default-extensions:\n    a\n    b\n"

  Hspec.it "sorts known extensions by name" $ do
    expectGilded
      "library\n default-extensions: DerivingVia BlockArguments"
      "library\n  default-extensions:\n    BlockArguments\n    DerivingVia\n"

  Hspec.it "does not sort disabling extensions after enabling ones" $ do
    expectGilded
      "library\n default-extensions: StrictData NoStrictData"
      "library\n  default-extensions:\n    NoStrictData\n    StrictData\n"

  Hspec.it "sorts unknown extensions with known ones" $ do
    expectGilded
      "library\n default-extensions: LambdaCase Imaginary"
      "library\n  default-extensions:\n    Imaginary\n    LambdaCase\n"

  Hspec.it "sorts unknown disabling extensions with known ones" $ do
    expectGilded
      "library\n default-extensions: NoLambdaCase NoImaginary"
      "library\n  default-extensions:\n    NoImaginary\n    NoLambdaCase\n"

  Hspec.it "sorts extensions (issue 29)" $ do
    expectGilded
      "library\n default-extensions: Arrows Imaginary1 NoCPP NoImaginary2 NoUnboxedTuples UnicodeSyntax"
      "library\n  default-extensions:\n    Arrows\n    Imaginary1\n    NoCPP\n    NoImaginary2\n    NoUnboxedTuples\n    UnicodeSyntax\n"

  Hspec.it "sorts other-extensions" $ do
    expectGilded
      "library\n other-extensions: b a"
      "library\n  other-extensions:\n    a\n    b\n"

  Hspec.it "sorts extensions" $ do
    expectGilded
      "library\n extensions: b a"
      "library\n  extensions:\n    a\n    b\n"

  Hspec.it "sorts build-depends" $ do
    expectGilded
      "library\n build-depends: q , p == 1"
      "library\n  build-depends:\n    p ==1,\n    q\n"

  Hspec.it "adds a trailing comma to build-depends when possible" $ do
    expectGilded
      "cabal-version: 2.2\nlibrary\n build-depends: p == 1, q == 2"
      "cabal-version: 2.2\n\nlibrary\n  build-depends:\n    p ==1,\n    q ==2,\n"

  Hspec.it "does not add a trailing comma to build-depends with one element" $ do
    expectGilded
      "cabal-version: 2.2\nlibrary\n build-depends: p"
      "cabal-version: 2.2\n\nlibrary\n  build-depends: p\n"

  Hspec.it "removes duplicate options" $ do
    -- This is kind of silly because there's only one possible foreign library
    -- option.
    expectGilded
      "foreign-library x\n options: standalone standalone"
      "foreign-library x\n  options: standalone\n"

  Hspec.it "does not sort hs-source-dirs" $ do
    expectGilded
      "library\n hs-source-dirs: b a"
      "library\n  hs-source-dirs:\n    b\n    a\n"

  Hspec.it "does not sort code-generators" $ do
    expectGilded
      "test-suite x\n code-generators: b, a"
      "test-suite x\n  code-generators:\n    b,\n    a\n"

  Hspec.it "adds a trailing comma to code-generators when possible" $ do
    expectGilded
      "cabal-version: 2.2\ntest-suite x\n code-generators: a, b"
      "cabal-version: 2.2\n\ntest-suite x\n  code-generators:\n    a,\n    b,\n"

  Hspec.it "does not add a trailing comma to code-generators with one element" $ do
    expectGilded
      "cabal-version: 2.2\ntest-suite x\n code-generators: a"
      "cabal-version: 2.2\n\ntest-suite x\n  code-generators: a\n"

  Hspec.it "does not sort cpp-options" $ do
    expectGilded
      "library\n cpp-options: b a"
      "library\n  cpp-options:\n    b\n    a\n"

  Hspec.it "does not sort asm-options" $ do
    expectGilded
      "library\n asm-options: b a"
      "library\n  asm-options:\n    b\n    a\n"

  Hspec.it "does not sort cmm-options" $ do
    expectGilded
      "library\n cmm-options: b a"
      "library\n  cmm-options:\n    b\n    a\n"

  Hspec.it "does not sort cc-options" $ do
    expectGilded
      "library\n cc-options: b a"
      "library\n  cc-options:\n    b\n    a\n"

  Hspec.it "does not sort cxx-options" $ do
    expectGilded
      "library\n cxx-options: b a"
      "library\n  cxx-options:\n    b\n    a\n"

  Hspec.it "does not sort ld-options" $ do
    expectGilded
      "library\n ld-options: b a"
      "library\n  ld-options:\n    b\n    a\n"

  Hspec.it "does not sort hsc2hs-options" $ do
    expectGilded
      "library\n hsc2hs-options: b a"
      "library\n  hsc2hs-options:\n    b\n    a\n"

  Hspec.it "sorts extra-libraries" $ do
    expectGilded
      "library\n extra-libraries: b a"
      "library\n  extra-libraries:\n    a\n    b\n"

  Hspec.it "sorts extra-libraries-static" $ do
    expectGilded
      "library\n extra-libraries-static: b a"
      "library\n  extra-libraries-static:\n    a\n    b\n"

  Hspec.it "sorts extra-ghci-libraries" $ do
    expectGilded
      "library\n extra-ghci-libraries: b a"
      "library\n  extra-ghci-libraries:\n    a\n    b\n"

  Hspec.it "sorts extra-bundled-libraries" $ do
    expectGilded
      "library\n extra-bundled-libraries: b a"
      "library\n  extra-bundled-libraries:\n    a\n    b\n"

  Hspec.it "sorts extra-library-flavours" $ do
    expectGilded
      "library\n extra-library-flavours: b a"
      "library\n  extra-library-flavours:\n    a\n    b\n"

  Hspec.it "sorts extra-dynamic-library-flavours" $ do
    expectGilded
      "library\n extra-dynamic-library-flavours: b a"
      "library\n  extra-dynamic-library-flavours:\n    a\n    b\n"

  Hspec.it "sorts extra-lib-dirs" $ do
    expectGilded
      "library\n extra-lib-dirs: g f"
      "library\n  extra-lib-dirs:\n    f\n    g\n"

  Hspec.it "sorts extra-lib-dirs-static" $ do
    expectGilded
      "library\n extra-lib-dirs-static: g f"
      "library\n  extra-lib-dirs-static:\n    f\n    g\n"

  Hspec.it "sorts include-dirs" $ do
    expectGilded
      "library\n include-dirs: g f"
      "library\n  include-dirs:\n    f\n    g\n"

  Hspec.it "sorts includes" $ do
    expectGilded
      "library\n includes: g f"
      "library\n  includes:\n    f\n    g\n"

  Hspec.it "sorts autogen-includes" $ do
    expectGilded
      "library\n autogen-includes: g f"
      "library\n  autogen-includes:\n    f\n    g\n"

  Hspec.it "sorts install-includes" $ do
    expectGilded
      "library\n install-includes: g f"
      "library\n  install-includes:\n    f\n    g\n"

  Hspec.it "sorts mixins" $ do
    expectGilded
      "library\n mixins: q (M, N as O), p"
      "library\n  mixins:\n    p,\n    q (M, N as O)\n"

  Hspec.it "does not sort ghc-options" $ do
    expectGilded
      "library\n ghc-options: b a"
      "library\n  ghc-options:\n    b\n    a\n"

  Hspec.it "does not sort ghcjs-options" $ do
    expectGilded
      "library\n ghcjs-options: b a"
      "library\n  ghcjs-options:\n    b\n    a\n"

  Hspec.it "does not sort ghc-prof-options" $ do
    expectGilded
      "library\n ghc-prof-options: b a"
      "library\n  ghc-prof-options:\n    b\n    a\n"

  Hspec.it "does not sort ghcjs-prof-options" $ do
    expectGilded
      "library\n ghcjs-prof-options: b a"
      "library\n  ghcjs-prof-options:\n    b\n    a\n"

  Hspec.it "does not sort ghc-shared-options" $ do
    expectGilded
      "library\n ghc-shared-options: b a"
      "library\n  ghc-shared-options:\n    b\n    a\n"

  Hspec.it "does not sort ghcjs-shared-options" $ do
    expectGilded
      "library\n ghcjs-shared-options: b a"
      "library\n  ghcjs-shared-options:\n    b\n    a\n"

  Hspec.it "sorts setup-depends" $ do
    expectGilded
      "custom-setup\n setup-depends: b, a"
      "custom-setup\n  setup-depends:\n    a,\n    b\n"

  Hspec.it "discovers an exposed module" $ do
    expectDiscover
      [(".", ["M.hs"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers an other module" $ do
    expectDiscover
      [(".", ["M.hs"])]
      "library\n -- cabal-gild: discover .\n other-modules:"
      "library\n  -- cabal-gild: discover .\n  other-modules: M\n"

  Hspec.it "discovers a nested module" $ do
    expectDiscover
      [(".", [FilePath.combine "N" "O.hs"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: N.O\n"

  Hspec.it "discovers multiple modules" $ do
    expectDiscover
      [(".", ["M.hs", "N.hs"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules:\n    M\n    N\n"

  Hspec.it "discovers a .lhs file" $ do
    expectDiscover
      [(".", ["M.lhs"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .gc file" $ do
    expectDiscover
      [(".", ["M.gc"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .chs file" $ do
    expectDiscover
      [(".", ["M.chs"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .hsc file" $ do
    expectDiscover
      [(".", ["M.hsc"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .y file" $ do
    expectDiscover
      [(".", ["M.y"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .ly file" $ do
    expectDiscover
      [(".", ["M.ly"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .x file" $ do
    expectDiscover
      [(".", ["M.x"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .cpphs file" $ do
    expectDiscover
      [(".", ["M.cpphs"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .hsig file" $ do
    expectDiscover
      [(".", ["M.hsig"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a .lhsig file" $ do
    expectDiscover
      [(".", ["M.lhsig"])]
      "library\n -- cabal-gild: discover .\n exposed-modules:"
      "library\n  -- cabal-gild: discover .\n  exposed-modules: M\n"

  Hspec.it "discovers a signature" $ do
    expectDiscover
      [(".", ["S.hsig"])]
      "library\n -- cabal-gild: discover .\n signatures:"
      "library\n  -- cabal-gild: discover .\n  signatures: S\n"

  Hspec.it "ignores discover pragma separated by comment" $ do
    expectGilded
      "library\n -- cabal-gild: discover .\n -- foo\n exposed-modules: M"
      "library\n  -- cabal-gild: discover .\n  -- foo\n  exposed-modules: M\n"

  Hspec.it "ignores misplaced discover pragma" $ do
    expectGilded
      "-- cabal-gild: discover .\nname: p"
      "-- cabal-gild: discover .\nname: p\n"

  Hspec.it "ignores unknown pragma" $ do
    expectGilded
      "-- cabal-gild: unknown"
      "-- cabal-gild: unknown\n"

  Hspec.it "discovers from multiple directories" $ do
    expectDiscover
      [("d", ["M.hs"]), ("e", ["N.hs"])]
      "library\n -- cabal-gild: discover d e\n exposed-modules:"
      "library\n  -- cabal-gild: discover d e\n  exposed-modules:\n    M\n    N\n"

  Hspec.it "parses an empty brace section" $ do
    expectGilded
      "s{}"
      "s\n"

  Hspec.it "parses a brace section with a layout field" $ do
    expectGilded
      "s{f:x}"
      "s\n  f: x\n"

  Hspec.it "parses a brace section with a brace field" $ do
    expectGilded
      "s{f:{x}}"
      "s\n  f: x\n"

  Hspec.it "strips blanks from a layout field in a brace section" $ do
    expectGilded
      "s { f : x } "
      "s\n  f: x\n"

  Hspec.it "strips blanks from a brace field in a brace section" $ do
    expectGilded
      "s { f : { x } } "
      "s\n  f: x\n"

  Hspec.it "parses a brace section with multiple fields" $ do
    expectGilded
      "s { f : { x } g : { y } } "
      "s\n  f: x\n  g: y\n"

  Hspec.it "parses a nested brace section" $ do
    expectGilded
      "s{t{}}"
      "s\n  t\n"

expectGilded :: (Stack.HasCallStack) => String -> String -> Hspec.Expectation
expectGilded input expected = do
  let (a, s, w) =
        runTest
          (Gild.mainWith "" [])
          (Map.singleton Nothing $ String.toUtf8 input, Map.empty)
          Map.empty
  a `Hspec.shouldBe` Right ()
  w `Hspec.shouldBe` []
  actual <- case Map.toList s of
    [(Nothing, x)] -> pure x
    _ -> fail $ "impossible: " <> show s
  actual `Hspec.shouldBe` String.toUtf8 expected
  -- After formatting, the output should not change if we format it again.
  expectStable actual

expectStable :: (Stack.HasCallStack) => ByteString.ByteString -> Hspec.Expectation
expectStable input = do
  let (a, s, w) =
        runTest
          (Gild.mainWith "" [])
          (Map.singleton Nothing input, Map.empty)
          Map.empty
  a `Hspec.shouldBe` Right ()
  w `Hspec.shouldBe` []
  output <- case Map.toList s of
    [(Nothing, x)] -> pure x
    _ -> fail $ "impossible: " <> show s
  output `Hspec.shouldBe` input

expectDiscover :: [(FilePath, [FilePath])] -> String -> String -> Hspec.Expectation
expectDiscover files input expected = do
  let (a, s, w) =
        runTest
          (Gild.mainWith "" [])
          ( Map.singleton Nothing $ String.toUtf8 input,
            Map.fromList $ fmap (\(d, fs) -> (d, FilePath.combine d <$> fs)) files
          )
          Map.empty
  a `Hspec.shouldBe` Right ()
  w `Hspec.shouldBe` []
  actual <- case Map.toList s of
    [(Nothing, x)] -> pure x
    _ -> fail $ "impossible: " <> show s
  actual `Hspec.shouldBe` String.toUtf8 expected

newtype Problem = Problem
  { unProblem :: Exception.SomeException
  }
  deriving (Show)

instance Eq Problem where
  (==) = Function.on (==) show

type Test = TestT Identity.Identity

runTest :: Test a -> R -> S -> (Either E a, S, W)
runTest t r = Identity.runIdentity . RWST.runRWST (ExceptT.runExceptT $ runTestT t) r

type E = Problem

type R = (S, Map.Map FilePath [FilePath])

type S = Map.Map (Maybe FilePath) ByteString.ByteString

type W = [String]

newtype TestT m a = TestT
  { runTestT :: ExceptT.ExceptT E (RWST.RWST R W S m) a
  }
  deriving (Applicative, Functor, Monad)

instance (Monad m) => MonadLog.MonadLog (TestT m) where
  log = TestT . Trans.lift . RWST.tell . pure

instance (Monad m) => MonadRead.MonadRead (TestT m) where
  read k = do
    m <- TestT . Trans.lift . RWST.asks $ Map.lookup k . fst
    case m of
      Nothing -> Exception.throwM . userError $ "read " <> show k
      Just x -> pure x

instance (Monad m) => Exception.MonadThrow (TestT m) where
  throwM = TestT . ExceptT.throwE . Problem . Exception.toException

instance (Monad m) => MonadWalk.MonadWalk (TestT m) where
  walk p = do
    m <- TestT . Trans.lift . RWST.asks $ Map.lookup p . snd
    case m of
      Nothing -> Exception.throwM . userError $ "walk " <> show p
      Just x -> pure x

instance (Monad m) => MonadWrite.MonadWrite (TestT m) where
  write k = TestT . Trans.lift . RWST.modify . Map.insert k
