module MCS51.CPU where

import Clash.Prelude
import MCS51.Core
import MCS51.InstructionSet
import MCS51.ALU
import Core.Memory (RomUnit, RamUnit)

-- ---------------------------------------------------------------------------
-- CPU pipeline stage
-- ---------------------------------------------------------------------------

-- | Multi-cycle pipeline stages.
data Stage
    = SStart                          -- warm-up: present PC to ROM
    | SFetch1                         -- waiting for byte 0
    | SFetch2  MCS51Word              -- have b0, waiting for b1 (2- or 3-byte)
    | SFetch3  MCS51Word MCS51Word    -- have b0+b1, waiting for b2 (3-byte)
    | SMemRead Instruction            -- waiting for XRAM read (MOVX) response
    deriving (Generic, NFDataX, Show, Eq)

data CPUState = CPUState
    { cpuCore  :: CoreData
    , cpuStage :: Stage
    } deriving (Generic, NFDataX, Show)

type BusOut =
    ( MCS51Addr                        -- code ROM byte address
    , Maybe MCS51Addr                  -- XRAM read address
    , Maybe (MCS51Addr, MCS51Word)     -- XRAM write
    )

-- ---------------------------------------------------------------------------
-- CPU state machine
-- ---------------------------------------------------------------------------

cpuStep :: CPUState
        -> (MCS51Word, MCS51Word, Maybe MCS51Addr)
        -- ^ (code ROM byte in, XRAM data in, interrupt vector)
        -> (CPUState, BusOut)

-- Warm-up: present PC, move to SFetch1.
cpuStep (CPUState core SStart) _ =
    ( CPUState core SFetch1
    , (pc core, Nothing, Nothing) )

-- Byte 0 arrives; check for interrupt first.
cpuStep (CPUState core SFetch1) (b0, _, irqVec) =
    case irqVec of
        Just vecPC | ie core .&. 0x80 /= 0 ->
            -- Accept interrupt: clear IE.EA, push current PC, jump to vector.
            let core' = core { ie = ie core .&. 0x7F }
            in doCall (pc core') vecPC core'
        _ ->
            case instrBytes b0 of
                1 -> dispatch (decodeInstruction b0 0 0) (pc core + 1) core
                _ -> ( CPUState core (SFetch2 b0)
                     , (pc core + 1, Nothing, Nothing) )

-- Byte 1 arrives.
cpuStep (CPUState core (SFetch2 b0)) (b1, _, _) =
    case instrBytes b0 of
        2 -> dispatch (decodeInstruction b0 b1 0) (pc core + 2) core
        _ -> ( CPUState core (SFetch3 b0 b1)
             , (pc core + 2, Nothing, Nothing) )

-- Byte 2 arrives (completing a 3-byte instruction).
cpuStep (CPUState core (SFetch3 b0 b1)) (b2, _, _) =
    dispatch (decodeInstruction b0 b1 b2) (pc core + 3) core

-- XRAM read response arrives (MOVX read).
cpuStep (CPUState core (SMemRead instr)) (_, dataIn, _) =
    execute instr (Just dataIn) (pc core) core

-- ---------------------------------------------------------------------------
-- Dispatch: route a decoded instruction
-- ---------------------------------------------------------------------------

-- | @dispatch instr seqPC core@ — handle an instruction whose sequential
--   (fall-through) PC is @seqPC@.
dispatch :: Instruction -> MCS51Addr -> CoreData -> (CPUState, BusOut)
dispatch instr seqPC core = case instr of
    -- CALL family: push return address onto IRAM stack, jump to target.
    LcallAddr tgt -> doCall seqPC tgt core
    AcallAddr tgt ->
        -- ACALL: target = (seqPC & 0xF800) | (tgt & 0x07FF)
        doCall seqPC ((seqPC .&. 0xF800) .|. (tgt .&. 0x07FF)) core

    -- RET/RETI: pop return address from IRAM stack.
    Ret  -> doRet False core
    Reti -> doRet True  core

    -- Normal instruction (possibly needs XRAM read).
    _ -> case mcs51Read instr core of
            Just xramAddr ->
                ( CPUState core (SMemRead instr)
                , (seqPC, Just xramAddr, Nothing) )
            Nothing ->
                execute instr Nothing seqPC core

-- ---------------------------------------------------------------------------
-- Execute: compute new state, issue optional XRAM write, advance PC
-- ---------------------------------------------------------------------------

execute :: Instruction -> Maybe MCS51Word -> MCS51Addr -> CoreData
        -> (CPUState, BusOut)
execute instr mval seqPC core =
    let core1    = mcs51Compute instr mval core
        writeSpec = mcs51Write instr core   -- use PRE-compute state for address
        nextPC   = case mcs51Jump instr seqPC core1 of
                       Just tgt -> tgt
                       Nothing  -> seqPC
        newCore  = core1 { pc = nextPC }
    in ( CPUState newCore SFetch1
       , (nextPC, Nothing, writeSpec) )

-- ---------------------------------------------------------------------------
-- CALL / RET — use IRAM stack directly (no external memory bus needed)
-- ---------------------------------------------------------------------------

-- | Push a 16-bit return address onto the IRAM stack and jump.
--   8051 convention: SP++ then write PCL, SP++ then write PCH.
doCall :: MCS51Addr -> MCS51Addr -> CoreData -> (CPUState, BusOut)
doCall retPC targetPC core =
    let lo   = truncateB retPC :: MCS51Word
        hi   = truncateB (retPC `shiftR` 8) :: MCS51Word
        sp1  = sp core + 1
        sp2  = sp core + 2
        c1   = writeIram core sp1 lo
        c2   = writeIram c1   sp2 hi
        c3   = c2 { sp = sp2, pc = targetPC }
    in ( CPUState c3 SFetch1
       , (targetPC, Nothing, Nothing) )

-- | Pop a 16-bit return address from the IRAM stack.
--   8051 convention: read [SP] → PCH, SP--; read [SP] → PCL, SP--.
doRet :: Bool -> CoreData -> (CPUState, BusOut)
doRet isReti core =
    let hiAddr = sp core
        loAddr = sp core - 1
        pcHi   = readIram core hiAddr
        pcLo   = readIram core loAddr
        retPC  = (zeroExtend pcHi `shiftL` 8) .|. zeroExtend pcLo
        ie'    = if isReti then ie core .|. 0x80 else ie core
        c'     = core { sp = sp core - 2, pc = retPC, ie = ie' }
    in ( CPUState c' SFetch1
       , (retPC, Nothing, Nothing) )

-- ---------------------------------------------------------------------------
-- Top-level synthesisable CPU
-- ---------------------------------------------------------------------------

-- | MCS-51 CPU core.  Connect to synchronous code ROM and external data RAM.
--
--   @irqVec@: interrupt vector (byte address) to jump to when an interrupt
--   is accepted.  Acceptance condition: irqVec = Just v AND IE.EA = 1.
--   On acceptance: IE.EA is cleared, return address pushed, CPU jumps to v.
--   RETI restores IE.EA and pops the return address.
mcs51Core
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe MCS51Addr)                -- interrupt vector in
    -> Signal dom MCS51Word                        -- code ROM byte in
    -> Signal dom MCS51Word                        -- XRAM data in
    -> ( Signal dom MCS51Addr                      -- code ROM address out
       , Signal dom (Maybe MCS51Addr)              -- XRAM read address out
       , Signal dom (Maybe (MCS51Addr, MCS51Word)) -- XRAM write out
       )
mcs51Core irqVec codeIn dataIn = (codeAddr, dataRdAddr, dataWr)
  where
    out      = mealy cpuStep (CPUState zeroState SStart)
                     (bundle (codeIn, dataIn, irqVec))
    codeAddr   = fmap (\(a, _, _) -> a) out
    dataRdAddr = fmap (\(_, b, _) -> b) out
    dataWr     = fmap (\(_, _, c) -> c) out

-- | Wire mcs51Core to blockRAM-style memories, closing the feedback loop.
mcs51SoC
    :: HiddenClockResetEnable dom
    => Signal dom (Maybe MCS51Addr)
    -> RomUnit dom MCS51Addr MCS51Word
    -> RamUnit dom MCS51Addr MCS51Word
    -> ( Signal dom MCS51Addr
       , Signal dom (Maybe MCS51Addr)
       , Signal dom (Maybe (MCS51Addr, MCS51Word))
       )
mcs51SoC irqVec codeRom xRam = (codeAddr, dataRdAddr, dataWr)
  where
    (codeAddr, dataRdAddr, dataWr) = mcs51Core irqVec codeIn dataIn
    codeIn = codeRom codeAddr
    dataIn = xRam (maybe 0 id <$> dataRdAddr) dataWr
