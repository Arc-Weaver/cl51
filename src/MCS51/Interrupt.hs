module MCS51.Interrupt
    ( interruptArbiter
    ) where

import Clash.Prelude
import MCS51.Core (MCS51Addr)
import qualified Core.Periph.Interrupt as I

-- | MCS-51 alias: priority interrupt arbiter over byte addresses.
--   Re-exports the generic arbiter with @MCS51Addr@ fixed as the vector type.
interruptArbiter
    :: KnownNat n
    => Vec n (Signal dom Bool, MCS51Addr)
    -> Signal dom Bool
    -> Signal dom (Maybe MCS51Addr)
interruptArbiter = I.interruptArbiter
