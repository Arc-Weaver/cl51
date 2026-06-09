module MCS51.Exec where

import Clash.Prelude
import MCS51.Core
import MCS51.InstructionSet
import MCS51.ALU
import MCS51.CPU (CPUState(..), cpuStep, Stage(..))

-- | Run instructions sequentially, ignoring PC-changing instructions.
--   Useful for testing linear sequences of instructions.
runLinear :: [MCS51Word] -> CoreData -> CoreData
runLinear [] core = core
runLinear (b0:bs) core =
    case instrBytes b0 of
        1 -> runLinear bs (step (decodeInstruction b0 0 0) core 1)
        2 -> case bs of
                 (b1:rest) -> runLinear rest (step (decodeInstruction b0 b1 0) core 2)
                 []        -> core
        _ -> case bs of
                 (b1:b2:rest) -> runLinear rest (step (decodeInstruction b0 b1 b2) core 3)
                 [_]          -> core
                 []           -> core
  where
    step instr c n = (mcs51Compute instr Nothing c) { pc = pc c + n }

-- | Run a program using the PC to address into program memory, honouring
--   jumps and branches. Terminates when pc >= stopAt.
runWithPC
    :: [MCS51Word]      -- program memory (byte-addressed from 0)
    -> MCS51Addr        -- stop when pc >= this
    -> CoreData
    -> CoreData
runWithPC mem stopAt core
    | pc core >= stopAt = core
    | otherwise =
        let i  = fromIntegral (pc core) :: Int
            b0 = fetch i
            n  = instrBytes b0
        in case n of
            1 -> runWithPC mem stopAt $ step (decodeInstruction b0 0 0) core (pc core + 1)
            2 -> let b1 = fetch (i + 1)
                 in runWithPC mem stopAt $ step (decodeInstruction b0 b1 0) core (pc core + 2)
            _ -> let b1 = fetch (i + 1)
                     b2 = fetch (i + 2)
                 in runWithPC mem stopAt $ step (decodeInstruction b0 b1 b2) core (pc core + 3)
  where
    fetch j   = maybe 0 id (listAt mem j)
    step instr c seqNext =
        let c'    = mcs51Compute instr Nothing c
            nextPC = case mcs51Jump instr seqNext c' of
                         Just tgt -> tgt
                         Nothing  -> seqNext
        in c' { pc = nextPC }

-- | Simulate the full CPU pipeline for @nCycles@ clock cycles.
--   The code ROM is a pure byte-addressed function.
--   XRAM always returns 0 (sufficient for write-only programs).
--   @irqs@ is the interrupt vector to present each cycle; padded with Nothing.
runPipeline
    :: (MCS51Addr -> MCS51Word)        -- byte-addressed code ROM
    -> [Maybe MCS51Addr]               -- interrupt vector per cycle
    -> Int                             -- total cycles to run
    -> CPUState
    -> CPUState
runPipeline codeRom irqs nCycles initState = go nCycles irqs initState 0 0
  where
    go 0 _  s _         _         = s
    go n is s pendCode  _pendData =
        let irq  = case is of { (i:_) -> i; [] -> Nothing }
            rest = case is of { (_:r) -> r; [] -> [] }
            (s', (nextCode, _, _)) = cpuStep s (pendCode, 0, irq)
        in go (n-1) rest s' (codeRom nextCode) 0

-- Safe list index; returns Nothing for out-of-bounds.
listAt :: [a] -> Int -> Maybe a
listAt []     _ = Nothing
listAt (x:_)  0 = Just x
listAt (_:xs) n = listAt xs (n - 1)
