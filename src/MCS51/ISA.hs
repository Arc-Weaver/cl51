-- | MCS-51 ISA definition using the ISACLE ISA DSL.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module MCS51.ISA
    ( module MCS51.ISA.Types
    , module MCS51.ISA.Arith
    , module MCS51.ISA.Move
    , module MCS51.ISA.Branch
    , mcs51IrqBody
    , mcs51ISA
    ) where

import Prelude hiding (Word)
import Hdl.Bits
import Isacle.ISA
import MCS51.ISA.Types
import MCS51.ISA.Arith
import MCS51.ISA.Move
import MCS51.ISA.Branch

-- ---------------------------------------------------------------------------
-- Interrupt body
-- ---------------------------------------------------------------------------

-- | MCS-51 interrupt service entry sequence.
-- Gates on the global interrupt enable (IE.7 / IEA), pushes the 16-bit PC
-- as two bytes (low then high) onto the 8-bit stack, then jumps to the
-- externally-supplied vector address.
mcs51IrqBody :: (MCS51 m, MonadIRQ m, KnownNat (IrqAddrW m)) => m ()
mcs51IrqBody = do
    irqGate (readFlag mcsIEA)
    pcR  <- cpu mcsPC
    spR  <- cpu mcsSP
    pc   <- readReg pcR
    eight <- litC 8
    lo   <- resizeBits pc              -- Unsigned 16 → Unsigned 8
    push spR lo
    hiRaw <- aluOp PShiftR pc eight
    hi   <- resizeBits hiRaw           -- Unsigned 16 → Unsigned 8
    push spR hi
    vec   <- irqVector
    vecPC <- resizeBits vec             -- Unsigned (IrqAddrW m) → Unsigned 16
    writeReg pcR vecPC

-- ---------------------------------------------------------------------------
-- ISA definition
-- ---------------------------------------------------------------------------

mcs51ISA :: (MCS51 m, MonadIRQ m, KnownNat (IrqAddrW m)) => ISADef m
mcs51ISA = defineISA ISADef
    { isaPc            = SomeCPURegister <$> cpu mcsPC
    , isaInterruptBody = Just mcs51IrqBody
    , isaReset         = do
        resetReg  mcsA   0x00
        resetReg  mcsB   0x00
        resetReg  mcsSP  0x07
        resetReg  mcsDPL 0x00
        resetReg  mcsDPH 0x00
        resetReg  mcsIE  0x00
        resetReg  mcsIP  0x00
        resetFlag mcsCY  Lo
        resetFlag mcsAC  Lo
        resetFlag mcsF0  Lo
        resetFlag mcsRS1 Lo
        resetFlag mcsRS0 Lo
        resetFlag mcsOV  Lo
        resetFlag mcsF1  Lo
        resetFlag mcsP   Lo
    , isaInstrs =
        [ nopDef
        -- Arithmetic
        , addARnDef, addADirDef, addAImmDef
        , addcARnDef, addcAImmDef
        , subbARnDef, subbAImmDef
        , incADef, incRnDef, incDirDef
        , decADef, decRnDef, decDirDef
        , mulABDef, divABDef
        -- Data movement
        , movARnDef, movADirDef, movAImmDef
        , movRnADef, movDirADef
        , movRnImmDef, movDirImmDef, movDirDirDef
        , pushDirDef, popDirDef
        , xchARnDef, xchADirDef
        -- Control flow
        , sjmpDef, ljmpDef, ajmpDef
        , jcDef, jncDef, jzDef, jnzDef
        , djnzRnDef, djnzDirDef
        , lcallDef, acallDef
        , retDef, retiDef
        ]
    }
