-- @createDomain@ generates orphan-instance warnings; suppress them.
{-# OPTIONS_GHC -Wno-orphans #-}

module Example.Project where

import Clash.Prelude
import MCS51.Core       (MCS51Addr, MCS51Word)
import MCS51.CPU        (mcs51Core)
import Core.Periph.Interrupt (interruptArbiter)
import Core.Periph.GPIO (gpioUnit)
import Core.TH          (loadBin8)

-- ---------------------------------------------------------------------------
-- Clock domain
-- ---------------------------------------------------------------------------

createDomain vSystem{vName="Dom10MHz", vPeriod=hzToPeriod 10e6}

-- ---------------------------------------------------------------------------
-- Program ROM
-- ---------------------------------------------------------------------------

-- | MCS-51 code ROM loaded at compile time from the assembled binary.
--   The ROM is byte-addressed; each element is one 8-bit fetch word.
testProgram :: Vec 256 MCS51Word
testProgram = $(loadBin8 "example/Example/program.bin")

-- ---------------------------------------------------------------------------
-- Memory map
-- ---------------------------------------------------------------------------

-- GPIO Port A: PIN=0xFF10, DDR=0xFF11, PORT=0xFF12
-- These are in the XRAM space (accessed via MOVX).
gpioABase :: MCS51Addr
gpioABase = 0xFF10

-- XRAM: 2 KB at 0x0000-0x07FF
xramBase :: MCS51Addr
xramBase = 0x0000

xramWords :: Int
xramWords = 2048

inGPIO_A :: MCS51Addr -> Bool
inGPIO_A a = a >= gpioABase && a < gpioABase + 3

inXRAM :: MCS51Addr -> Bool
inXRAM a = a >= xramBase && a < xramBase + fromIntegral xramWords

-- ---------------------------------------------------------------------------
-- Periodic timer: fires one cycle pulse every 2^n clock cycles
-- ---------------------------------------------------------------------------

-- | Free-running counter that asserts True for one cycle each time it wraps.
--   Period = 2^n clock cycles.  Used as the interrupt source for the example.
periodicTimer :: forall dom n . (HiddenClockResetEnable dom, KnownNat n)
              => SNat n -> Signal dom Bool
periodicTimer SNat = fmap (== maxBound) cnt
  where
    cnt :: Signal dom (Unsigned n)
    cnt = register 0 (fmap (+1) cnt)

-- ---------------------------------------------------------------------------
-- SoC
-- ---------------------------------------------------------------------------

-- | Full SoC wiring.
--
--   Returns @(portOut, ddrOut)@ for GPIO Port A:
--
--     portOut - the PORT latch; connect to the O pin of each IOBUF.
--     ddrOut  - data-direction register.  A '1' bit means OUTPUT.
soc :: forall dom . HiddenClockResetEnable dom
    => Signal dom MCS51Word                          -- GPIO A physical pin inputs
    -> (Signal dom MCS51Word, Signal dom MCS51Word)  -- (PORT latch, DDR / OE)
soc gpioIn = (portOut, ddrOut)
  where
    -- ── Interrupt controller ─────────────────────────────────────────────────
    -- Timer fires every 32 cycles (SNat @5 → period = 2^5).
    -- The CPU gates acceptance on IE.EA internally.
    timerReq = periodicTimer (SNat @5)
    irqVec   = interruptArbiter ((timerReq, 0x0003) :> Nil) (pure True)

    -- ── CPU ──────────────────────────────────────────────────────────────────
    (codeAddr, rdAddr, wr) = mcs51Core irqVec codeIn dataIn

    -- ── Code ROM (synchronous, 1-cycle latency via blockRam) ─────────────────
    codeIn = blockRam testProgram
                 (toRomIdx <$> codeAddr)
                 (pure Nothing)
      where
        toRomIdx :: Unsigned 16 -> Index 256
        toRomIdx = fromIntegral

    -- ── XRAM (2 KB at 0x0000) ────────────────────────────────────────────────
    xramRdIdx :: Signal dom (Index 2048)
    xramRdIdx = fmap rdIdx rdAddr
      where
        rdIdx Nothing  = 0
        rdIdx (Just a) = fromIntegral (if inXRAM a then a - xramBase else 0)

    xramWr :: Signal dom (Maybe (Index 2048, MCS51Word))
    xramWr = fmap wrRoute wr
      where
        wrRoute Nothing         = Nothing
        wrRoute (Just (a, v))
            | inXRAM a          = Just (fromIntegral (a - xramBase), v)
            | otherwise         = Nothing

    xramRd = blockRam (replicate (SNat @2048) 0) xramRdIdx xramWr

    -- ── GPIO Port A ──────────────────────────────────────────────────────────
    (gpioRd, portOut, ddrOut) = gpioUnit gpioABase gpioIn rdAddr wr

    -- ── Read-data mux ────────────────────────────────────────────────────────
    lastWasGPIO :: Signal dom Bool
    lastWasGPIO = register False (maybe False inGPIO_A <$> rdAddr)

    dataIn = mux lastWasGPIO gpioRd xramRd

-- ---------------------------------------------------------------------------
-- Synthesis top entity
-- ---------------------------------------------------------------------------

{-# ANN topEntity
  (Synthesize
    { t_name   = "mcs51_soc"
    , t_inputs = [ PortName "clk"
                 , PortName "rst_n"
                 , PortName "en"
                 , PortName "gpio_a_in"
                 ]
    , t_output = PortProduct ""
                     [ PortName "gpio_a_port"
                     , PortName "gpio_a_ddr"
                     ]
    }) #-}

{-# OPAQUE topEntity #-}

topEntity :: Clock Dom10MHz
          -> Reset Dom10MHz
          -> Enable Dom10MHz
          -> Signal Dom10MHz MCS51Word
          -> (Signal Dom10MHz MCS51Word, Signal Dom10MHz MCS51Word)
topEntity = exposeClockResetEnable soc
