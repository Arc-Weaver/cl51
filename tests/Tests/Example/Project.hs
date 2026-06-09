module Tests.Example.Project where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import qualified Clash.Prelude as C

import Example.Project (testProgram)
import MCS51.Core      (MCS51Word)

-- | The test program Vec has the right size (256 bytes).
prop_programSize :: H.Property
prop_programSize = H.withTests 1 . H.property $
    H.assert (length (C.toList testProgram) == 256)

-- | All bytes in the test program are valid 8-bit values (trivially true
--   for Unsigned 8 but checks the list is non-empty and well-formed).
prop_programNonEmpty :: H.Property
prop_programNonEmpty = H.withTests 1 . H.property $
    H.assert (not (null (C.toList testProgram)))

-- | Verify the first three bytes are 0x02, 0x00, 0x?? (LJMP instruction).
--   The reset vector at byte 0 should be LJMP (opcode 0x02).
prop_programStartsWithLjmp :: H.Property
prop_programStartsWithLjmp = H.withTests 1 . H.property $ do
    let bytes = C.toList testProgram :: [MCS51Word]
    H.assert (not (null bytes))
    -- The program starts with LJMP (0x02) for the reset vector.
    head bytes H.=== 0x02

accumTests :: TestTree
accumTests = $(testGroupGenerator)
