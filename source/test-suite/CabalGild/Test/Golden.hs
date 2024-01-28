module CabalGild.Test.Golden (tests) where

import CabalGild (cabalGild)
import CabalGild.Monad (runCabalGild)
import CabalGild.Options (defaultOptions)
import CabalGild.Prelude
import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import System.FilePath ((-<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.HUnit as HUnit

tests :: TestTree
tests =
  testGroup
    "tests"
    [ goldenTest' "cabal-fmt",
      goldenTest' "Cabal",
      goldenTest' "Cabal-notab",
      goldenTest' "simple-example",
      goldenTest' "tree-diff",
      goldenTest' "fragment-missing",
      goldenTest' "fragment-empty",
      goldenTest' "fragment-wrong-field",
      goldenTest' "fragment-wrong-type",
      goldenTest' "fragment-multiple",
      goldenTest' "fragment-section",
      goldenTest' "issue69",
      goldenTest' "issue29"
    ]

goldenTest' :: String -> TestTree
goldenTest' n =
  testGroup
    n
    [ HUnit.testCase "successfully formats" $ do
        input <- BS.readFile inputPath
        (output, warnings) <-
          either Exception.throwIO pure
            . runCabalGild files defaultOptions
            $ cabalGild inputPath input
        expected <- readFile goldenPath
        let actual = unlines (fmap ("-- " <>) warnings) <> output
        actual HUnit.@?= expected,
      HUnit.testCase "round trips" $ do
        input <- BS.readFile inputPath
        (expected, _) <-
          either Exception.throwIO pure
            . runCabalGild files defaultOptions
            $ cabalGild inputPath input
        (actual, _) <-
          either Exception.throwIO pure
            . runCabalGild files defaultOptions
            . cabalGild inputPath
            $ toUTF8BS expected
        actual HUnit.@?= expected
    ]
  where
    goldenPath = "fixtures" </> n -<.> "format"
    inputPath = "fixtures" </> n -<.> "cabal"

files :: Map.Map FilePath BS.ByteString
files =
  Map.fromList
    [ p "empty.fragment" "",
      p
        "build-depends.fragment"
        "build-depends: base, doctest >=0.15 && <0.17, QuickCheck >=2.12 && <2.13, simple-example, template-haskell",
      p
        "tested-with.fragment"
        "tested-with: GHC ==8.0.2",
      p
        "common.fragment"
        "common deps\n  build-depends: base, bytestring, containers\n  ghc-options: -Wall",
      p
        "multiple.fragment"
        "build-depends: base\nghc-options: -Wall",
      p ("cbits" </> "header.h") "...",
      p ("cbits" </> "source1.c") "...",
      p ("cbits" </> "source2.c") "...",
      p ("cbits" </> "sub" </> "source3.c") "..."
    ]
  where
    p x y = (x, BS8.pack y)
