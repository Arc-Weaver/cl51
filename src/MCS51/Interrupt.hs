module MCS51.Interrupt
    ( interruptArbiter
    ) where

import Clash.Prelude
import MCS51.Core (MCS51Addr)

-- | Combinational priority interrupt arbiter.
--
--   Sources are in priority order: index 0 = highest priority.  When multiple
--   sources are active simultaneously the lowest-index request wins.
--
--   The output is gated by the caller-supplied @iEnabled@ signal (IE.EA).
--   If @iEnabled@ is False the output is always Nothing regardless of requests,
--   matching the MCS-51 global interrupt enable semantics.
--
--   The standard MCS-51 interrupt vectors (byte addresses):
--     0x0003  INT0  — external interrupt 0
--     0x000B  TF0   — timer 0 overflow
--     0x0013  INT1  — external interrupt 1
--     0x001B  TF1   — timer 1 overflow
--     0x0023  RI/TI — UART serial interrupt
--
--   Usage:
--
--     interruptArbiter
--         (    (int0Req,  0x0003)   -- INT0
--          :> (tf0Req,   0x000B)   -- Timer 0
--          :> (int1Req,  0x0013)   -- INT1
--          :> (tf1Req,   0x001B)   -- Timer 1
--          :> (uartReq,  0x0023)   -- UART
--          :> Nil )
--         ieEnabled
interruptArbiter
    :: KnownNat n
    => Vec n (Signal dom Bool, MCS51Addr)   -- (request line, vector byte address)
    -> Signal dom Bool                      -- IE.EA (global interrupt enable)
    -> Signal dom (Maybe MCS51Addr)
interruptArbiter sources iEnabled = liftA2 gate iEnabled winner
  where
    candidates = map toCandidate sources
    winner     = foldr (liftA2 firstJust) (pure Nothing) candidates

    toCandidate (req, vec) = fmap (\r -> if r then Just vec else Nothing) req

    firstJust (Just a) _ = Just a
    firstJust Nothing  b = b

    gate True  w = w
    gate False _ = Nothing
