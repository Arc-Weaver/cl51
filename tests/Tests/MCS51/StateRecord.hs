{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Regression tests for the 8051 state/flag records (C1/C2): the PSW, IE and
-- whole-state HdlType records have the expected widths and round-trip through
-- bits, locking in the flagRec/record migration.
module Tests.MCS51.StateRecord where

import Prelude
import Data.Proxy   (Proxy(..))
import GHC.TypeLits (natVal)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import Hdl.Types (Width, toBits, fromBits)
import MCS51.ISA.Types (Psw(..), Ie(..), Mcs51State(..))

prop_psw_ie_widths :: H.Property
prop_psw_ie_widths = H.withTests 1 . H.property $ do
    natVal (Proxy @(Width Psw)) H.=== 8
    natVal (Proxy @(Width Ie))  H.=== 8

prop_state_width :: H.Property
prop_state_width = H.withTests 1 . H.property $
    natVal (Proxy @(Width Mcs51State)) H.=== 80

-- A representative state with distinct field values; round-trips through bits.
sampleState :: Mcs51State
sampleState = Mcs51State 0xAA 0xBB 0x07 0x12 0x34 0x56
    (Ie 1 0 0 1 0 0 0 1) (Psw 1 0 0 0 0 0 0 1) 0x1234

prop_state_roundtrip :: H.Property
prop_state_roundtrip = H.withTests 1 . H.property $ do
    let s = fromBits (toBits sampleState) :: Mcs51State
    a  s H.=== 0xAA
    pc s H.=== 0x1234
    -- nested bit-map records survive
    pCY (psw s) H.=== 1
    pP  (psw s) H.=== 1
    iEA (ie s)  H.=== 1
    iES (ie s)  H.=== 1

stateRecordTests :: TestTree
stateRecordTests = $(testGroupGenerator)
