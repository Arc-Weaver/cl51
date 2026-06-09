module Tests.MCS51.Interrupt where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import qualified Clash.Prelude as C

import MCS51.Core    (MCS51Addr, zeroState, CoreData(..))
import MCS51.Interrupt (interruptArbiter)
import MCS51.CPU     (CPUState(..), Stage(..), cpuStep)
import MCS51.Exec    (runPipeline)

-- ---------------------------------------------------------------------------
-- Arbiter helpers
-- ---------------------------------------------------------------------------

-- | Evaluate a 1-source arbiter for one cycle.
arbiter1 :: Bool -> Bool -> MCS51Addr -> Maybe MCS51Addr
arbiter1 req ie vec =
    let srcs :: C.Vec 1 (C.Signal C.System Bool, MCS51Addr)
        srcs = (C.pure req, vec) C.:> C.Nil
        out  :: C.Signal C.System (Maybe MCS51Addr)
        out  = interruptArbiter srcs (C.pure ie)
    in C.sampleN 2 out !! 1

-- | Evaluate a 2-source arbiter for one cycle.
arbiter2 :: (Bool, MCS51Addr) -> (Bool, MCS51Addr) -> Bool -> Maybe MCS51Addr
arbiter2 (r0, v0) (r1, v1) ie =
    let srcs :: C.Vec 2 (C.Signal C.System Bool, MCS51Addr)
        srcs = (C.pure r0, v0) C.:> (C.pure r1, v1) C.:> C.Nil
        out  :: C.Signal C.System (Maybe MCS51Addr)
        out  = interruptArbiter srcs (C.pure ie)
    in C.sampleN 2 out !! 1

-- ---------------------------------------------------------------------------
-- Arbiter unit tests
-- ---------------------------------------------------------------------------

prop_arbiter_accepts_asserted_request :: H.Property
prop_arbiter_accepts_asserted_request = H.withTests 1 . H.property $
    arbiter1 True True 0x0003 H.=== Just 0x0003

prop_arbiter_no_output_when_not_requested :: H.Property
prop_arbiter_no_output_when_not_requested = H.withTests 1 . H.property $
    arbiter1 False True 0x0003 H.=== Nothing

prop_arbiter_blocked_when_ie_false :: H.Property
prop_arbiter_blocked_when_ie_false = H.withTests 1 . H.property $
    arbiter1 True False 0x0003 H.=== Nothing

prop_arbiter_first_source_has_priority :: H.Property
prop_arbiter_first_source_has_priority = H.withTests 1 . H.property $
    arbiter2 (True, 0x0003) (True, 0x000B) True H.=== Just 0x0003

prop_arbiter_second_source_wins_when_first_clear :: H.Property
prop_arbiter_second_source_wins_when_first_clear = H.withTests 1 . H.property $
    arbiter2 (False, 0x0003) (True, 0x000B) True H.=== Just 0x000B

prop_arbiter_nothing_when_no_requests :: H.Property
prop_arbiter_nothing_when_no_requests = H.withTests 1 . H.property $
    arbiter2 (False, 0x0003) (False, 0x000B) True H.=== Nothing

-- ---------------------------------------------------------------------------
-- CPU interrupt acceptance (cpuStep-level)
-- ---------------------------------------------------------------------------

-- | A CPU state at SFetch1 with IE.EA (global enable) set (IE = 0x80).
iFetch1 :: CoreData -> CPUState
iFetch1 core = CPUState (core { ie = 0x80 }) SFetch1

-- | A CPU state at SFetch1 with IE.EA cleared.
noIFetch1 :: CoreData -> CPUState
noIFetch1 core = CPUState (core { ie = 0x00 }) SFetch1

-- IE.EA is cleared the moment the interrupt is accepted.
prop_cpu_irq_clears_ea_bit :: H.Property
prop_cpu_irq_clears_ea_bit = H.withTests 1 . H.property $ do
    let s0 = iFetch1 zeroState
    let (s1, _) = cpuStep s0 (0x00, 0x00, Just (0x0003 :: MCS51Addr))
    -- IE.EA (bit 7) should be cleared
    (ie (cpuCore s1) C..&. 0x80) H.=== (0 :: C.Unsigned 8)

-- When IE.EA=0, IRQ vector is ignored and the CPU fetches normally.
prop_cpu_irq_ignored_when_disabled :: H.Property
prop_cpu_irq_ignored_when_disabled = H.withTests 1 . H.property $ do
    let s0 = noIFetch1 zeroState
    let (s1, _) = cpuStep s0 (0x00, 0x00, Just (0x0003 :: MCS51Addr))
    -- CPU should stay in SFetch1 (NOP decoded, no interrupt accepted)
    cpuStage s1 H.=== SFetch1
    -- IE.EA remains 0
    (ie (cpuCore s1) C..&. 0x80) H.=== (0 :: C.Unsigned 8)

-- After acceptance the CPU completes the CALL push and jumps to the vector
-- in a single step (CALL/interrupt is now single-cycle in the CPU).
prop_cpu_irq_jumps_to_vector :: H.Property
prop_cpu_irq_jumps_to_vector = H.withTests 1 . H.property $ do
    let s0 = iFetch1 zeroState
    let (s1, _) = cpuStep s0 (0x00, 0x00, Just (0x0003 :: MCS51Addr))
    -- After one step the CPU is already at SFetch1 pointing at the vector.
    cpuStage s1     H.=== SFetch1
    pc (cpuCore s1) H.=== (0x0003 :: C.Unsigned 16)

-- ---------------------------------------------------------------------------
-- RETI restores IE.EA
-- ---------------------------------------------------------------------------

-- | Run a 1-byte instruction from a small program.
execReti :: CoreData -> CoreData
execReti core =
    -- RETI is a single-cycle instruction: it reads the return address from
    -- IRAM (iram[sp] for hi, iram[sp-1] for lo) and restores IE.EA.
    let s0 = CPUState (core { ie = 0x00, sp = 0x09 }) SFetch1
        (s1, _) = cpuStep s0 (0x32, 0x00, Nothing)
    in cpuCore s1

prop_reti_restores_ea :: H.Property
prop_reti_restores_ea = H.withTests 1 . H.property $ do
    let finalCore = execReti zeroState
    (ie finalCore C..&. 0x80) H.=== (0x80 :: C.Unsigned 8)

-- ---------------------------------------------------------------------------
-- Full interrupt → ISR → RETI round-trip
-- ---------------------------------------------------------------------------

prop_full_interrupt_reti_cycle :: H.Property
prop_full_interrupt_reti_cycle = H.withTests 1 . H.property $ do
    let s0 = iFetch1 zeroState

    -- Step 1: IRQ arrives → CPU accepts interrupt in one step (push PC,
    -- clear IE.EA, jump to vector).
    let (s1, _) = cpuStep s0 (0x00, 0x00, Just (0x0003 :: MCS51Addr))
    (ie (cpuCore s1) C..&. 0x80) H.=== (0 :: C.Unsigned 8)
    cpuStage s1     H.=== SFetch1
    pc (cpuCore s1) H.=== (0x0003 :: C.Unsigned 16)

    -- Step 2: NOP at ISR entry → advances pc to 0x0004.
    let (s2, _) = cpuStep s1 (0x00, 0x00, Nothing)
    cpuStage s2 H.=== SFetch1

    -- Step 3: RETI at 0x0004 → single-cycle: pops return address (0x0000)
    -- from IRAM and restores IE.EA.
    let (s3, _) = cpuStep s2 (0x32, 0x00, Nothing)
    cpuStage s3             H.=== SFetch1
    pc (cpuCore s3)         H.=== (0x0000 :: C.Unsigned 16)
    (ie (cpuCore s3) C..&. 0x80) H.=== (0x80 :: C.Unsigned 8)

-- ---------------------------------------------------------------------------
-- Pipeline-level interrupt test (runPipeline)
-- ---------------------------------------------------------------------------

-- A program that sets IE = 0x83 (EA + EX1 + EX0) and then loops.
-- byte 0: MOV IE, #0x83 = 0x75 0xA8 0x83  (3 bytes)
-- byte 3: SJMP .-2 = 0x80 0xFD            (2 bytes, loops back to byte 3)
-- byte 5: RETI = 0x32                     (1 byte, acts as ISR)

seiLoopProg :: MCS51Addr -> C.Unsigned 8
seiLoopProg 0 = 0x75   -- MOV direct, #imm
seiLoopProg 1 = 0xA8   -- IE address
seiLoopProg 2 = 0x83   -- value: EA=1, ET1=1, EX0=1
seiLoopProg 3 = 0x80   -- SJMP
seiLoopProg 4 = 0xFD   -- offset -3 (loops back to byte 3)
seiLoopProg 5 = 0x32   -- RETI (ISR)
seiLoopProg _ = 0x00   -- NOP

prop_pipeline_irq_redirects_pc :: H.Property
prop_pipeline_irq_redirects_pc = H.withTests 1 . H.property $ do
    -- Inject IRQ vector 0x0005 from cycle 10 onward.
    let irqs     = replicate 10 Nothing ++ repeat (Just 0x0005)
        initCPU  = CPUState zeroState SStart
        finalCPU = runPipeline seiLoopProg irqs 20 initCPU
    -- After 20 cycles the CPU should have jumped to or past the ISR at 0x0005.
    H.assert (pc (cpuCore finalCPU) >= 0x0005)

interruptTests :: TestTree
interruptTests = $(testGroupGenerator)
