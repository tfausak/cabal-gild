import qualified CabalGild.Test.Golden
import qualified CabalGild.Test.Interval
import qualified Test.Tasty

main :: IO ()
main =
  Test.Tasty.defaultMain $
    Test.Tasty.testGroup
      "cabal-gild"
      [ CabalGild.Test.Golden.tests,
        CabalGild.Test.Interval.tests
      ]
