{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module MCS51.ISA.Types
    ( MCS51ALU(..)
    , Psw(..)
    , Ie(..)
    , Mcs51State(..)
    , mcs51CPUDef
    , MCS51
    , mcs51FlagAt
    , pcAdvance2
    , pcAdvance3
    , readOp
    , readOp16
    , addArith
    , subbArith
    , stubFlags
    , regField
    , pageField
    , regAddr
    ) where

import Prelude hiding (Word)
import GHC.Generics (Generic, Rep)

import Hdl.Bits hiding ((!!), zeroExtend, signExtend, truncateB, bitCoerce, slice, add, mul, shiftL, shiftR, xor, (.&.), (.|.))
import Hdl.Types (HdlType(..), GWidth, genericToBits, genericFromBits)
import Isacle.ISA

-- ---------------------------------------------------------------------------
-- ALU definition record
-- ---------------------------------------------------------------------------

data MCS51ALU = MCS51ALU
    { mcsA    :: CPURegister (Unsigned 8)    -- Accumulator          (SFR 0xE0)
    , mcsB    :: CPURegister (Unsigned 8)    -- B register           (SFR 0xF0)
    , mcsSP   :: CPURegister (Unsigned 8)    -- Stack pointer        (SFR 0x81)
    , mcsDPL  :: CPURegister (Unsigned 8)    -- Data pointer low     (SFR 0x82)
    , mcsDPH  :: CPURegister (Unsigned 8)    -- Data pointer high    (SFR 0x83)
    , mcsIE   :: CPURegister (Unsigned 8)    -- Interrupt enable     (SFR 0xA8)
    , mcsIP   :: CPURegister (Unsigned 8)    -- Interrupt priority   (SFR 0xB8)
    , mcsPSW  :: CPURegister (Unsigned 8)    -- Program status word  (SFR 0xD0)
    , mcsPC   :: CPURegister (Unsigned 16)   -- Program counter
    -- PSW flags, MSB-first: CY AC F0 RS1 RS0 OV F1 P
    , mcsCY   :: CPUFlag          -- Carry        (bit 7)
    , mcsAC   :: CPUFlag          -- Aux carry    (bit 6)
    , mcsF0   :: CPUFlag          -- User flag 0  (bit 5)
    , mcsRS1  :: CPUFlag          -- Bank select 1 (bit 4)
    , mcsRS0  :: CPUFlag          -- Bank select 0 (bit 3)
    , mcsOV   :: CPUFlag          -- Overflow     (bit 2)
    , mcsF1   :: CPUFlag          -- User flag 1  (bit 1)
    , mcsP    :: CPUFlag          -- Parity       (bit 0)
    -- IE.EA — interrupt global enable flag (bit 7 of IE)
    , mcsIEA  :: CPUFlag
    }

-- ---------------------------------------------------------------------------
-- PSW and IE as bit-map record HdlTypes (C2/C5)
-- Declaration order is MSB-first, so the first field is bit 7 … last is bit 0.
-- 'flagRec' derives each flag's bit position from the record layout — no
-- separate bit-index list.
-- ---------------------------------------------------------------------------

-- | Program status word (SFR 0xD0): CY AC F0 RS1 RS0 OV F1 P, MSB-first.
data Psw = Psw
    { pCY  :: Bit   -- ^ bit 7 — carry
    , pAC  :: Bit   -- ^ bit 6 — auxiliary carry
    , pF0  :: Bit   -- ^ bit 5 — user flag 0
    , pRS1 :: Bit   -- ^ bit 4 — register bank select 1
    , pRS0 :: Bit   -- ^ bit 3 — register bank select 0
    , pOV  :: Bit   -- ^ bit 2 — overflow
    , pF1  :: Bit   -- ^ bit 1 — user flag 1
    , pP   :: Bit   -- ^ bit 0 — parity
    } deriving Generic

instance HdlType Psw where
    type Width Psw = GWidth (Rep Psw)
    toBits   = genericToBits
    fromBits = genericFromBits

-- | Interrupt enable (SFR 0xA8): EA – ET2 ES ET1 EX1 ET0 EX0, MSB-first.
data Ie = Ie
    { iEA  :: Bit   -- ^ bit 7 — global interrupt enable
    , iIE6 :: Bit   -- ^ bit 6 — (reserved / IE.6)
    , iET2 :: Bit   -- ^ bit 5 — timer 2 interrupt enable
    , iES  :: Bit   -- ^ bit 4 — serial interrupt enable
    , iET1 :: Bit   -- ^ bit 3 — timer 1 interrupt enable
    , iEX1 :: Bit   -- ^ bit 2 — external interrupt 1 enable
    , iET0 :: Bit   -- ^ bit 1 — timer 0 interrupt enable
    , iEX0 :: Bit   -- ^ bit 0 — external interrupt 0 enable
    } deriving Generic

instance HdlType Ie where
    type Width Ie = GWidth (Rep Ie)
    toBits   = genericToBits
    fromBits = genericFromBits

-- | The 8051 architectural state as one recursive 'HdlType' record (C1:
-- "core satisfies HdlType"): scalar SFRs plus the nested 'Ie' and 'Psw' bit-map
-- records and the 16-bit PC. Width = 6·8 + 8 + 8 + 16 = 80. The handle record
-- ('MCS51ALU') still drives synthesis; this is the structural view (additive).
data Mcs51State = Mcs51State
    { a   :: Unsigned 8
    , b   :: Unsigned 8
    , sp  :: Unsigned 8
    , dpl :: Unsigned 8
    , dph :: Unsigned 8
    , ip  :: Unsigned 8
    , ie  :: Ie            -- nested bit-map record
    , psw :: Psw           -- nested bit-map record
    , pc  :: Unsigned 16
    } deriving Generic

instance HdlType Mcs51State where
    type Width Mcs51State = GWidth (Rep Mcs51State)
    toBits   = genericToBits
    fromBits = genericFromBits

-- 'readField'/'writeField' reach a register by its state field name.

-- ---------------------------------------------------------------------------
-- CPUDef
--
-- Internal address space (DataAddr ~ IExpr (Unsigned 8)):
--   0x00-0x7F  IRAM (lower 128 bytes; register banks live at 0x00-0x1F)
--   0x80-0xFF  SFRs (intercepted by aliasReg)
--
-- Register banks R0-R7 are not modelled as a regFile; they live physically
-- in IRAM, so instruction bodies use readMem/writeMem with an address
-- computed as bankBase(PSW.RS1:RS0) + regIndex.
-- ---------------------------------------------------------------------------

mcs51CPUDef :: CPUDef MCS51ALU
mcs51CPUDef = do
    endianness LittleEndian
    a'   <- reg "A"   byte
    b'   <- reg "B"   byte
    sp'  <- reg "SP"  byte
    dpl' <- reg "DPL" byte
    dph' <- reg "DPH" byte
    ip'  <- reg "IP"  byte
    pc'  <- reg "PC"  w16
    psw' <- reg "PSW" byte
    -- PSW flags are bits of PSW (MSB-first): CY@7 AC@6 F0@5 RS1@4 RS0@3 OV@2 F1@1 P@0.
    cy  <- newFlag "CY"  (psw' ! 7)
    ac  <- newFlag "AC"  (psw' ! 6)
    f0  <- newFlag "F0"  (psw' ! 5)
    rs1 <- newFlag "RS1" (psw' ! 4)
    rs0 <- newFlag "RS0" (psw' ! 3)
    ov  <- newFlag "OV"  (psw' ! 2)
    f1  <- newFlag "F1"  (psw' ! 1)
    p   <- newFlag "P"   (psw' ! 0)
    ie'  <- reg "IE"  byte
    -- IE flags (MSB-first): EA@7 IE6@6 ET2@5 ES@4 ET1@3 EX1@2 ET0@1 EX0@0. All
    -- eight are declared so the status-register bit map is fully covered (only
    -- EA is referenced by the ISA today).
    iea <- newFlag "EA"  (ie' ! 7)
    _   <- newFlag "IE6" (ie' ! 6)
    _   <- newFlag "ET2" (ie' ! 5)
    _   <- newFlag "ES"  (ie' ! 4)
    _   <- newFlag "ET1" (ie' ! 3)
    _   <- newFlag "EX1" (ie' ! 2)
    _   <- newFlag "ET0" (ie' ! 1)
    _   <- newFlag "EX0" (ie' ! 0)
    aliasReg a'   0xE0
    aliasReg b'   0xF0
    aliasReg sp'  0x81
    aliasReg dpl' 0x82
    aliasReg dph' 0x83
    aliasReg ie'  0xA8
    aliasReg ip'  0xB8
    aliasReg psw' 0xD0
    pure MCS51ALU
        { mcsA   = a',  mcsB  = b',   mcsSP  = sp'
        , mcsDPL = dpl', mcsDPH = dph', mcsIE = ie', mcsIP = ip'
        , mcsPSW = psw', mcsPC = pc'
        , mcsCY  = cy,  mcsAC  = ac,  mcsF0  = f0,  mcsRS1 = rs1
        , mcsRS0 = rs0, mcsOV  = ov,  mcsF1  = f1,  mcsP   = p
        , mcsIEA = iea
        }

-- ---------------------------------------------------------------------------
-- Constraint alias
-- ---------------------------------------------------------------------------

type MCS51 m = ( MonadHarvardALU m, AluDef m ~ MCS51ALU
               , Word m ~ IExpr (Unsigned 8), DataAddr m ~ IExpr (Unsigned 8)
               , CodeAddr m ~ IExpr (Unsigned 16), CodeWord m ~ IExpr (Unsigned 8) )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Map a PSW bit index (0=CY .. 7=P) to the corresponding CPUFlag.
mcs51FlagAt :: MCS51ALU -> Int -> CPUFlag
mcs51FlagAt alu 0 = mcsCY  alu
mcs51FlagAt alu 1 = mcsAC  alu
mcs51FlagAt alu 2 = mcsF0  alu
mcs51FlagAt alu 3 = mcsRS1 alu
mcs51FlagAt alu 4 = mcsRS0 alu
mcs51FlagAt alu 5 = mcsOV  alu
mcs51FlagAt alu 6 = mcsF1  alu
mcs51FlagAt alu _ = mcsP   alu

pcAdvance2, pcAdvance3 :: MCS51 m => m ()
pcAdvance2 = advance 2
pcAdvance3 = advance 3

advance :: MCS51 m => Integer -> m ()
advance n = do
    pcR <- cpu mcsPC
    p   <- readReg pcR
    k   <- litC (fromInteger n)
    writeReg pcR (p + k)

-- | Read the nth operand byte after the opcode (n=0 → byte immediately after
--   the opcode, n=1 → the byte after that).  PC must not have been modified
--   before this call.
readOp :: MCS51 m => Integer -> m (IExpr (Unsigned 8))
readOp n = do
    pcR  <- cpu mcsPC
    p    <- readReg pcR
    off  <- litC (n + 1)
    readCode (p + off)

-- | Read a big-endian 16-bit address from operand bytes 0 and 1
--   (byte 0 = high byte, byte 1 = low byte).
readOp16 :: MCS51 m => m (IExpr (Unsigned 16))
readOp16 = do
    hi    <- readOp 0
    lo    <- readOp 1
    eight <- litC (8 :: Integer)
    let hiW = shiftL (zeroExtend hi :: IExpr (Unsigned 16)) eight
    pure (hiW .|. zeroExtend lo)

-- ---------------------------------------------------------------------------
-- Encoding-DSL helpers (typed field placeholders, replacing string encodings)
-- ---------------------------------------------------------------------------

-- | An opcode with a 3-bit register selector in the low bits: @\<5 fixed\>rrr@
-- (the @rrr@\/@nnn@ register-select field).  Returns the field placeholder.
regField :: String -> Encoding (Field (Unsigned 3))
regField pre = do
    fixed pre
    field @(Unsigned 3)

-- | An opcode with a 3-bit page selector in the /high/ bits: @aaa\<5 fixed\>@
-- (ACALL\/AJMP).  Returns the field placeholder.
pageField :: String -> Encoding (Field (Unsigned 3))
pageField suf = do
    pg <- field @(Unsigned 3)
    fixed suf
    return pg

-- | The data-memory address of register @Rn@ selected by a 3-bit field: the
-- field value zero-extended to a full 8-bit internal-RAM address (R0–R7 of the
-- current bank live at 0x00–0x07).
regAddr :: Field (Unsigned 3) -> IExpr (Unsigned 8)
regAddr f = zeroExtend (immediateF f)

-- | Stub the arithmetic flags (CY, AC, OV, P) to zero — synthesis
-- placeholder until proper carry-chain logic is wired in.
stubFlags :: MCS51 m => m ()
stubFlags = do
    alu <- cpu id
    z   <- litC 0
    mapM_ (\f -> setFlag f z) [mcsCY alu, mcsAC alu, mcsOV alu, mcsP alu]

-- | XOR-reduce an 8-bit value to its parity bit (1 = odd number of ones).
parityBit :: MCS51 m => IExpr (Unsigned 8) -> m (IExpr Bool)
parityBit v = do
    four <- litC 4
    two  <- litC 2
    one  <- litC 1
    let p4 = xor v  (shiftR v  four)
        p2 = xor p4 (shiftR p4 two)
        p1 = xor p2 (shiftR p2 one)
    return (truncateB p1)

-- | Compute A + B + cyIn, set CY / AC / OV / P, return 8-bit result.
-- Uses a 9-bit literal anchor (z9) so the VHDL emitter infers a 9-bit sum
-- and preserves the carry-out bit.
addArith :: MCS51 m => IExpr (Unsigned 8) -> IExpr (Unsigned 8) -> IExpr Bool -> m (IExpr (Unsigned 8))
addArith a b cyIn = do
    -- 9-bit sum for CY: z9 forces the output wire to 9 bits.
    z9   <- litC 0
    let cy9  = (zeroExtend cyIn :: IExpr (Unsigned 9)) + z9
        bc9  = (zeroExtend b    :: IExpr (Unsigned 9)) + cy9
        sum9 = (zeroExtend a    :: IExpr (Unsigned 9)) + bc9
    eight <- litC 8
    let bit8 = shiftR sum9 eight
    writeFlag mcsCY (truncateB bit8)
    -- AC: nibble half-carry — (a&F) + (b&F) + cy in 8 bits; bit 4 = AC.
    mskF <- litC 0x0F
    four <- litC 4
    let alo = a .&. mskF
        blo = b .&. mskF
        hal = alo + (blo + (zeroExtend cyIn :: IExpr (Unsigned 8)))
        bit4 = shiftR hal four
    writeFlag mcsAC (truncateB bit4)
    let r = truncateB sum9 :: IExpr (Unsigned 8)
    -- OV: same-sign inputs, different-sign result.
    seven <- litC 7
    let a7  = shiftR a seven
        b7  = shiftR b seven
        r7  = shiftR r seven
        xab = xor a7 b7
        xar = xor a7 r7
    sns <- isZero xab
    let ov = (zeroExtend sns :: IExpr (Unsigned 8)) .&. xar
    writeFlag mcsOV (truncateB ov)
    writeFlag mcsP =<< parityBit r
    return r

-- | Compute A - B - cyIn (SUBB), set CY / AC / OV / P, return result.
-- Uses two's-complement: A + ~B + ~cyIn.  CY = borrow = NOT bit8 of 9-bit sum.
subbArith :: MCS51 m => IExpr (Unsigned 8) -> IExpr (Unsigned 8) -> IExpr Bool -> m (IExpr (Unsigned 8))
subbArith a b cyIn = do
    let notB = inv b
    notCy <- isZero (zeroExtend cyIn :: IExpr (Unsigned 8))
    -- 9-bit sum: A + ~B + ~cyIn
    z9    <- litC 0
    let ncy9 = (zeroExtend notCy :: IExpr (Unsigned 9)) + z9
        nb9  = (zeroExtend notB  :: IExpr (Unsigned 9)) + ncy9
        sum9 = (zeroExtend a     :: IExpr (Unsigned 9)) + nb9
    eight <- litC 8
    let bit8 = shiftR sum9 eight
    -- borrow when carry-out is absent (bit8 = 0)
    writeFlag mcsCY =<< isZero bit8
    -- AC: half-borrow from nibble; same complement trick.
    mskF   <- litC 0x0F
    four   <- litC 4
    let alo    = a    .&. mskF
        notBlo = notB .&. mskF
        hal    = alo + (notBlo + (zeroExtend notCy :: IExpr (Unsigned 8)))
        bit4   = shiftR hal four
    writeFlag mcsAC =<< isZero bit4
    let r = truncateB sum9 :: IExpr (Unsigned 8)
    -- OV: different-sign inputs, sign change in result.
    seven <- litC 7
    let a7  = shiftR a seven
        b7  = shiftR b seven
        r7  = shiftR r seven
        xab = xor a7 b7
        xar = xor a7 r7
        ov  = xab .&. xar
    writeFlag mcsOV (truncateB ov)
    writeFlag mcsP =<< parityBit r
    return r
