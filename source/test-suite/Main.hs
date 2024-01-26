import qualified CabalFmt.Test.Golden
import qualified CabalFmt.Test.Interval
import qualified Test.Tasty

main :: IO ()
main =
  Test.Tasty.defaultMain $
    Test.Tasty.testGroup
      "cabal-fmt"
      [ CabalFmt.Test.Golden.tests,
        CabalFmt.Test.Interval.tests
      ]
