import Prelude

import Test.Tasty

import qualified Tests.MCS51.ISASim

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.MCS51.ISASim.isaSimTests
  ]
