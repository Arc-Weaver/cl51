module MCS51.Core where

import Clash.Prelude
import MCS51.InstructionSet (Instruction, instrBytes, decodeInstruction)

type MCS51Word  = Unsigned 8    -- 8-bit data byte
type MCS51Addr  = Unsigned 16   -- 16-bit external / code address
type MCS51IAddr = Unsigned 8    -- 8-bit internal data address

-- ---------------------------------------------------------------------------
-- Program Status Word
-- ---------------------------------------------------------------------------

-- | PSW flags, SFR address 0xD0.
data PSW = PSW
    { psw_cy  :: Bit  -- bit 7: carry flag
    , psw_ac  :: Bit  -- bit 6: auxiliary carry
    , psw_f0  :: Bit  -- bit 5: user flag 0
    , psw_rs1 :: Bit  -- bit 4: register bank select 1
    , psw_rs0 :: Bit  -- bit 3: register bank select 0
    , psw_ov  :: Bit  -- bit 2: overflow flag
    , psw_f1  :: Bit  -- bit 1: user flag 1 (undefined in original 8051)
    , psw_p   :: Bit  -- bit 0: parity (ACC parity, set by hardware)
    } deriving (Generic, NFDataX, Show, Eq)

instance BitPack PSW where
    type BitSize PSW = 8
    pack p =
        pack (psw_cy p) ++# pack (psw_ac p) ++# pack (psw_f0  p) ++#
        pack (psw_rs1 p) ++# pack (psw_rs0 p) ++# pack (psw_ov p) ++#
        pack (psw_f1 p) ++# pack (psw_p p)
    unpack b = PSW
        { psw_cy  = unpack (slice d7 d7 b)
        , psw_ac  = unpack (slice d6 d6 b)
        , psw_f0  = unpack (slice d5 d5 b)
        , psw_rs1 = unpack (slice d4 d4 b)
        , psw_rs0 = unpack (slice d3 d3 b)
        , psw_ov  = unpack (slice d2 d2 b)
        , psw_f1  = unpack (slice d1 d1 b)
        , psw_p   = unpack (slice d0 d0 b)
        }

zeroPSW :: PSW
zeroPSW = PSW 0 0 0 0 0 0 0 0

-- | Byte index of the active register bank base (0x00, 0x08, 0x10, 0x18).
bankBase :: PSW -> Index 128
bankBase p = case (psw_rs1 p, psw_rs0 p) of
    (0, 0) -> 0x00
    (0, 1) -> 0x08
    (1, 0) -> 0x10
    _      -> 0x18

-- ---------------------------------------------------------------------------
-- CPU state
-- ---------------------------------------------------------------------------

-- | Full MCS-51 CPU state.
--
--   The internal data memory is split:
--     iram  — lower 128 bytes (0x00–0x7F):
--               0x00–0x1F  four register banks of R0–R7
--               0x20–0x2F  bit-addressable area
--               0x30–0x7F  general-purpose scratch
--   Special Function Registers are modelled as explicit fields and shadowed
--   into the logical SFR space on direct/bit accesses.
data CoreData = CoreData
    { acc  :: MCS51Word          -- SFR 0xE0: accumulator
    , breg :: MCS51Word          -- SFR 0xF0: B register (for MUL/DIV)
    , psw  :: PSW                -- SFR 0xD0: program status word
    , sp   :: MCS51Word          -- SFR 0x81: stack pointer (reset: 0x07)
    , dpl  :: MCS51Word          -- SFR 0x82: data pointer low
    , dph  :: MCS51Word          -- SFR 0x83: data pointer high
    , ie   :: MCS51Word          -- SFR 0xA8: interrupt enable
    , ip   :: MCS51Word          -- SFR 0xB8: interrupt priority
    , iram :: Vec 128 MCS51Word  -- internal data RAM 0x00–0x7F
    , pc   :: MCS51Addr          -- program counter (16-bit)
    } deriving (Generic, NFDataX, Show)

-- | Reset state: SP=0x07, all others zero, IRAM zeroed.
zeroState :: CoreData
zeroState = CoreData
    { acc  = 0
    , breg = 0
    , psw  = zeroPSW
    , sp   = 0x07
    , dpl  = 0
    , dph  = 0
    , ie   = 0
    , ip   = 0
    , iram = repeat 0
    , pc   = 0
    }

-- ---------------------------------------------------------------------------
-- Register bank helpers
-- ---------------------------------------------------------------------------

-- | Read R0–R7 from the current bank.
getReg :: CoreData -> Unsigned 3 -> MCS51Word
getReg c n = iram c !! (bankBase (psw c) + fromIntegral n)

-- | Write R0–R7 in the current bank.
setReg :: CoreData -> Unsigned 3 -> MCS51Word -> CoreData
setReg c n v =
    let addr = bankBase (psw c) + fromIntegral n
    in c { iram = replace addr v (iram c) }

-- | Read from IRAM (0x00–0x7F only).
readIram :: CoreData -> MCS51IAddr -> MCS51Word
readIram c a = iram c !! (fromIntegral a :: Index 128)

-- | Write to IRAM (0x00–0x7F only).
writeIram :: CoreData -> MCS51IAddr -> MCS51Word -> CoreData
writeIram c a v = c { iram = replace (fromIntegral a :: Index 128) v (iram c) }

-- | Read a byte from the direct-address space (0x00–0xFF).
--   0x00–0x7F → IRAM; 0x80–0xFF → SFR.
readDirect :: CoreData -> MCS51IAddr -> MCS51Word
readDirect c a
    | a <= 0x7F = readIram c a
    | a == 0x81 = sp c
    | a == 0x82 = dpl c
    | a == 0x83 = dph c
    | a == 0xA8 = ie c
    | a == 0xB8 = ip c
    | a == 0xD0 = unpack (pack (psw c))
    | a == 0xE0 = acc c
    | a == 0xF0 = breg c
    | otherwise = 0

-- | Write a byte to the direct-address space (0x00–0xFF).
writeDirect :: CoreData -> MCS51IAddr -> MCS51Word -> CoreData
writeDirect c a v
    | a <= 0x7F = writeIram c a v
    | a == 0x81 = c { sp   = v }
    | a == 0x82 = c { dpl  = v }
    | a == 0x83 = c { dph  = v }
    | a == 0xA8 = c { ie   = v }
    | a == 0xB8 = c { ip   = v }
    | a == 0xD0 = c { psw  = unpack (pack v) }
    | a == 0xE0 = c { acc  = v }
    | a == 0xF0 = c { breg = v }
    | otherwise = c

-- | DPTR as a 16-bit value.
getDptr :: CoreData -> MCS51Addr
getDptr c = zeroExtend (dph c) `shiftL` 8 .|. zeroExtend (dpl c)

-- | Set DPTR from a 16-bit value.
setDptr :: CoreData -> MCS51Addr -> CoreData
setDptr c v = c
    { dph = truncateB (v `shiftR` 8)
    , dpl = truncateB v
    }

-- ---------------------------------------------------------------------------
-- Instruction fetch/decode pipeline
-- ---------------------------------------------------------------------------

-- | Decode pipeline state for up to 3 bytes per instruction.
data DecodeState
    = AwaitFirst
    | AwaitSecond MCS51Word
    | AwaitThird  MCS51Word MCS51Word
    deriving (Generic, NFDataX, Show, Eq)

-- | One pipeline step: consume one byte from code memory.
--   Returns Nothing on stall cycles while collecting multi-byte instructions.
decodeStep :: DecodeState -> MCS51Word -> (DecodeState, Maybe Instruction)
decodeStep AwaitFirst b0 =
    case instrBytes b0 of
        1 -> (AwaitFirst,            Just (decodeInstruction b0 0 0))
        2 -> (AwaitSecond b0,        Nothing)
        _ -> (AwaitThird  b0 0,      Nothing)
decodeStep (AwaitSecond b0) b1 =
    case instrBytes b0 of
        2 -> (AwaitFirst,            Just (decodeInstruction b0 b1 0))
        _ -> (AwaitThird  b0 b1,     Nothing)
decodeStep (AwaitThird b0 b1) b2 =
    (AwaitFirst, Just (decodeInstruction b0 b1 b2))
