import Prelude

import Test.Tasty

import qualified Tests.MCS51.ISASim
import qualified Tests.MCS51.StateRecord

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.MCS51.ISASim.isaSimTests
  , Tests.MCS51.StateRecord.stateRecordTests
  ]
