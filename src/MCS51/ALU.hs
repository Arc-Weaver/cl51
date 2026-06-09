module MCS51.ALU where

import Clash.Prelude
import MCS51.Core
import MCS51.InstructionSet
-- ---------------------------------------------------------------------------
-- Bit addressing helpers
-- ---------------------------------------------------------------------------

-- | Decode a bit address (0x00–0x7F → bit-addressable IRAM 0x20–0x2F;
--   0x80–0xFF → SFR bit address, byte = bit[7:3], bit = bit[2:0]).
readBit :: CoreData -> BitAddr -> Bit
readBit c ba
    | ba <= 0x7F =
        let byteAddr = 0x20 + fromIntegral (ba `shiftR` 3) :: Unsigned 8
            bitIdx   = fromIntegral (ba .&. 7) :: Index 8
        in unpack (slice d0 d0 (pack (readDirect c byteAddr `shiftR` fromIntegral bitIdx)))
    | otherwise  =
        let byteAddr = fromIntegral (ba .&. 0xF8) :: Unsigned 8
            bitIdx   = fromIntegral (ba .&. 7) :: Index 8
        in unpack (slice d0 d0 (pack (readDirect c byteAddr `shiftR` fromIntegral bitIdx)))

-- | Write a bit in the bit-addressable space.
writeBit :: CoreData -> BitAddr -> Bit -> CoreData
writeBit c ba v
    | ba <= 0x7F =
        let byteAddr = 0x20 + fromIntegral (ba `shiftR` 3) :: Unsigned 8
            bitIdx   = fromIntegral (ba .&. 7) :: Int
            old      = readDirect c byteAddr
            new      = if v == 1
                       then old .|. (1 `shiftL` bitIdx)
                       else old .&. complement (1 `shiftL` bitIdx)
        in writeDirect c byteAddr new
    | otherwise  =
        let byteAddr = fromIntegral (ba .&. 0xF8) :: Unsigned 8
            bitIdx   = fromIntegral (ba .&. 7) :: Int
            old      = readDirect c byteAddr
            new      = if v == 1
                       then old .|. (1 `shiftL` bitIdx)
                       else old .&. complement (1 `shiftL` bitIdx)
        in writeDirect c byteAddr new

-- ---------------------------------------------------------------------------
-- Arithmetic helpers
-- ---------------------------------------------------------------------------

-- | 8-bit add with carry in and carry/auxiliary-carry/overflow out.
addWithCarry :: MCS51Word -> MCS51Word -> Bit
             -> (MCS51Word, Bit, Bit, Bit)
             -- ^ (result, carry, aux_carry, overflow)
addWithCarry a b cin =
    let a9  = zeroExtend a  :: Unsigned 9
        b9  = zeroExtend b  :: Unsigned 9
        c9  = zeroExtend (unpack (pack cin) :: Unsigned 1) :: Unsigned 9
        r9  = a9 + b9 + c9
        res = truncateB r9  :: MCS51Word
        cy  = unpack (slice d8 d8 (pack r9))
        -- auxiliary carry: carry out of bit 3
        a4  = zeroExtend (truncateB a :: Unsigned 4) :: Unsigned 5
        b4  = zeroExtend (truncateB b :: Unsigned 4) :: Unsigned 5
        c4  = zeroExtend (unpack (pack cin) :: Unsigned 1) :: Unsigned 5
        r4  = a4 + b4 + c4
        ac  = unpack (slice d4 d4 (pack r4))
        -- overflow: carry into bit 7 XOR carry out of bit 7
        a8  = zeroExtend (truncateB a :: Unsigned 7) :: Unsigned 8
        b8  = zeroExtend (truncateB b :: Unsigned 7) :: Unsigned 8
        c8  = zeroExtend (unpack (pack cin) :: Unsigned 1) :: Unsigned 8
        r8  = a8 + b8 + c8
        cin7 = unpack (slice d7 d7 (pack r8))
        ov  = unpack (pack (xor' cy cin7))
    in (res, cy, ac, ov)
  where
    xor' :: Bit -> Bit -> Bit
    xor' x y = unpack (pack x `xor` pack y)

-- | 8-bit subtract with borrow.  SUBB A, src: A = A - src - CY.
subbWithBorrow :: MCS51Word -> MCS51Word -> Bit
               -> (MCS51Word, Bit, Bit, Bit)
               -- ^ (result, new_carry/borrow, aux_carry, overflow)
subbWithBorrow a b bin =
    -- Subtraction as two's complement add: A + (~B) + (1 - CY)
    addWithCarry a (complement b) (complement bin)

-- | Compute even parity of an 8-bit word (for PSW.P).
parityBit :: MCS51Word -> Bit
parityBit w =
    let b = pack w :: BitVector 8
        p = slice d7 d7 b `xor` slice d6 d6 b `xor` slice d5 d5 b `xor`
            slice d4 d4 b `xor` slice d3 d3 b `xor` slice d2 d2 b `xor`
            slice d1 d1 b `xor` slice d0 d0 b
    in unpack p

-- | Update PSW flags after an ALU operation.
setFlags :: CoreData -> MCS51Word -> Bit -> Bit -> Bit -> CoreData
setFlags c res cy ac ov =
    let p   = parityBit res
        psw' = (psw c) { psw_cy = cy, psw_ac = ac, psw_ov = ov, psw_p = p }
    in c { acc = res, psw = psw' }

-- ---------------------------------------------------------------------------
-- Jump target computation
-- ---------------------------------------------------------------------------

-- | Compute the target PC for a relative branch.
--   The base is the address of the instruction following the branch.
branchTarget :: MCS51Addr -> Relative -> MCS51Addr
branchTarget base rel = base + fromIntegral rel

-- ---------------------------------------------------------------------------
-- MCS-51 ALU implementation
-- ---------------------------------------------------------------------------

-- | The external XRAM read address for MOVX instructions (if any).
--   Returns Nothing for instructions that don't need external memory access.
mcs51Read :: Instruction -> CoreData -> Maybe MCS51Addr
mcs51Read instr c = case instr of
    MovxA_dptr   -> Just (getDptr c)
    MovxA_ri  ri -> Just (zeroExtend (getReg c ri))
    _            -> Nothing

-- | Execute the instruction (compute new state from old state + optional read result).
mcs51Compute :: Instruction -> Maybe MCS51Word -> CoreData -> CoreData
mcs51Compute instr mval c = case instr of
    Nop -> c

    -- ── Arithmetic ──────────────────────────────────────────────────────────
    AddA_imm imm ->
        let (r, cy, ac, ov) = addWithCarry (acc c) imm 0
        in setFlags c r cy ac ov
    AddA_dir dir ->
        let (r, cy, ac, ov) = addWithCarry (acc c) (readDirect c dir) 0
        in setFlags c r cy ac ov
    AddA_ri ri ->
        let addr = getReg c ri
            v    = readDirect c addr
            (r, cy, ac, ov) = addWithCarry (acc c) v 0
        in setFlags c r cy ac ov
    AddA_rn rn ->
        let (r, cy, ac, ov) = addWithCarry (acc c) (getReg c rn) 0
        in setFlags c r cy ac ov

    AddcA_imm imm ->
        let (r, cy, ac, ov) = addWithCarry (acc c) imm (psw_cy (psw c))
        in setFlags c r cy ac ov
    AddcA_dir dir ->
        let (r, cy, ac, ov) = addWithCarry (acc c) (readDirect c dir) (psw_cy (psw c))
        in setFlags c r cy ac ov
    AddcA_ri ri ->
        let addr = getReg c ri
            v    = readDirect c addr
            (r, cy, ac, ov) = addWithCarry (acc c) v (psw_cy (psw c))
        in setFlags c r cy ac ov
    AddcA_rn rn ->
        let (r, cy, ac, ov) = addWithCarry (acc c) (getReg c rn) (psw_cy (psw c))
        in setFlags c r cy ac ov

    SubbA_imm imm ->
        let (r, cy, ac, ov) = subbWithBorrow (acc c) imm (psw_cy (psw c))
        in setFlags c r cy ac ov
    SubbA_dir dir ->
        let (r, cy, ac, ov) = subbWithBorrow (acc c) (readDirect c dir) (psw_cy (psw c))
        in setFlags c r cy ac ov
    SubbA_ri ri ->
        let addr = getReg c ri
            v    = readDirect c addr
            (r, cy, ac, ov) = subbWithBorrow (acc c) v (psw_cy (psw c))
        in setFlags c r cy ac ov
    SubbA_rn rn ->
        let (r, cy, ac, ov) = subbWithBorrow (acc c) (getReg c rn) (psw_cy (psw c))
        in setFlags c r cy ac ov

    MulAB ->
        let a16  = zeroExtend (acc c)  :: Unsigned 16
            b16  = zeroExtend (breg c) :: Unsigned 16
            prod = a16 * b16
            lo   = truncateB prod :: MCS51Word
            hi   = truncateB (prod `shiftR` 8) :: MCS51Word
            ov   = if hi /= 0 then 1 else 0
            p    = parityBit lo
        in c { acc  = lo
             , breg = hi
             , psw  = (psw c) { psw_cy = 0, psw_ov = ov, psw_p = p }
             }

    DivAB ->
        if breg c == 0
        then c { psw = (psw c) { psw_cy = 0, psw_ov = 1 } }  -- undefined result
        else let q = acc c `div` breg c
                 r = acc c `mod` breg c
                 p = parityBit q
             in c { acc  = q
                  , breg = r
                  , psw  = (psw c) { psw_cy = 0, psw_ov = 0, psw_p = p }
                  }

    DaA ->
        -- Decimal adjust: adjust ACC after BCD addition
        let a   = acc c
            cy0 = psw_cy (psw c)
            ac0 = psw_ac (psw c)
            lo  = a .&. 0x0F
            hi  = a `shiftR` 4
            (a1, cy1) = if lo > 9 || ac0 == 1
                        then let r9 = zeroExtend a + (6 :: Unsigned 9)
                             in (truncateB r9, unpack (slice d8 d8 (pack r9)) :: Bit)
                        else (a, 0)
            hi1 = a1 `shiftR` 4
            (a2, cy2) = if hi1 > 9 || cy0 == 1 || cy1 == 1
                        then let r9 = zeroExtend a1 + (0x60 :: Unsigned 9)
                             in (truncateB r9, unpack (slice d8 d8 (pack r9)) :: Bit)
                        else (a1, 0)
            newCy = unpack (pack cy1 .|. pack cy2 .|. pack cy0)
            p     = parityBit a2
        in c { acc = a2, psw = (psw c) { psw_cy = newCy, psw_p = p } }

    IncA ->
        let r = acc c + 1
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    IncDir dir ->
        let v = readDirect c dir + 1
        in writeDirect c dir v
    IncRi ri ->
        let addr = getReg c ri
            v    = readDirect c addr + 1
        in writeDirect c addr v
    IncRn rn ->
        setReg c rn (getReg c rn + 1)
    IncDptr ->
        setDptr c (getDptr c + 1)

    DecA ->
        let r = acc c - 1
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    DecDir dir ->
        let v = readDirect c dir - 1
        in writeDirect c dir v
    DecRi ri ->
        let addr = getReg c ri
            v    = readDirect c addr - 1
        in writeDirect c addr v
    DecRn rn ->
        setReg c rn (getReg c rn - 1)

    -- ── Logical ──────────────────────────────────────────────────────────────
    AnlA_imm imm ->
        let r = acc c .&. imm
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    AnlA_dir dir ->
        let r = acc c .&. readDirect c dir
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    AnlA_ri ri ->
        let v = readDirect c (getReg c ri)
            r = acc c .&. v
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    AnlA_rn rn ->
        let r = acc c .&. getReg c rn
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    AnlDir_A dir ->
        let v = readDirect c dir .&. acc c
        in writeDirect c dir v
    AnlDir_imm dir imm ->
        let v = readDirect c dir .&. imm
        in writeDirect c dir v

    OrlA_imm imm ->
        let r = acc c .|. imm
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    OrlA_dir dir ->
        let r = acc c .|. readDirect c dir
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    OrlA_ri ri ->
        let v = readDirect c (getReg c ri)
            r = acc c .|. v
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    OrlA_rn rn ->
        let r = acc c .|. getReg c rn
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    OrlDir_A dir ->
        let v = readDirect c dir .|. acc c
        in writeDirect c dir v
    OrlDir_imm dir imm ->
        let v = readDirect c dir .|. imm
        in writeDirect c dir v

    XrlA_imm imm ->
        let r = acc c `xor` imm
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    XrlA_dir dir ->
        let r = acc c `xor` readDirect c dir
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    XrlA_ri ri ->
        let v = readDirect c (getReg c ri)
            r = acc c `xor` v
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    XrlA_rn rn ->
        let r = acc c `xor` getReg c rn
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    XrlDir_A dir ->
        let v = readDirect c dir `xor` acc c
        in writeDirect c dir v
    XrlDir_imm dir imm ->
        let v = readDirect c dir `xor` imm
        in writeDirect c dir v

    ClrA ->
        c { acc = 0, psw = (psw c) { psw_p = 0 } }
    CplA ->
        let r = complement (acc c)
            p = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }

    RlA ->
        let a   = acc c
            msb = unpack (slice d7 d7 (pack a)) :: Bit
            r   = (a `shiftL` 1) .|. zeroExtend (unpack (pack msb) :: Unsigned 1)
            p   = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    RlcA ->
        let a   = acc c
            cy  = psw_cy (psw c)
            msb = unpack (slice d7 d7 (pack a)) :: Bit
            r   = (a `shiftL` 1) .|. zeroExtend (unpack (pack cy) :: Unsigned 1)
            p   = parityBit r
        in c { acc = r, psw = (psw c) { psw_cy = msb, psw_p = p } }
    RrA ->
        let a   = acc c
            lsb = unpack (slice d0 d0 (pack a)) :: Bit
            r   = (a `shiftR` 1) .|. (zeroExtend (unpack (pack lsb) :: Unsigned 1) `shiftL` 7)
            p   = parityBit r
        in c { acc = r, psw = (psw c) { psw_p = p } }
    RrcA ->
        let a   = acc c
            cy  = psw_cy (psw c)
            lsb = unpack (slice d0 d0 (pack a)) :: Bit
            r   = (a `shiftR` 1) .|. (zeroExtend (unpack (pack cy) :: Unsigned 1) `shiftL` 7)
            p   = parityBit r
        in c { acc = r, psw = (psw c) { psw_cy = lsb, psw_p = p } }

    SwapA ->
        let a = acc c
            r = (a `shiftL` 4) .|. (a `shiftR` 4)
        in c { acc = r }

    -- ── Data transfer ────────────────────────────────────────────────────────
    MovA_imm imm -> c { acc = imm, psw = (psw c) { psw_p = parityBit imm } }
    MovA_dir dir ->
        let v = readDirect c dir
        in c { acc = v, psw = (psw c) { psw_p = parityBit v } }
    MovA_ri  ri  ->
        let v = readDirect c (getReg c ri)
        in c { acc = v, psw = (psw c) { psw_p = parityBit v } }
    MovA_rn  rn  ->
        let v = getReg c rn
        in c { acc = v, psw = (psw c) { psw_p = parityBit v } }

    MovDir_A  dir        -> writeDirect c dir (acc c)
    MovDir_imm dir imm   -> writeDirect c dir imm
    MovDir_dir src dst   -> writeDirect c dst (readDirect c src)
    MovDir_ri  dir ri    -> writeDirect c dir (readDirect c (getReg c ri))
    MovDir_rn  dir rn    -> writeDirect c dir (getReg c rn)

    MovRi_A  ri          -> writeDirect c (getReg c ri) (acc c)
    MovRi_dir ri dir     -> writeDirect c (getReg c ri) (readDirect c dir)
    MovRi_imm ri imm     -> writeDirect c (getReg c ri) imm

    MovRn_A  rn          -> setReg c rn (acc c)
    MovRn_dir rn dir     -> setReg c rn (readDirect c dir)
    MovRn_imm rn imm     -> setReg c rn imm

    MovDptr addr         -> setDptr c addr

    -- MOVX: external memory; mval carries the read result (if any)
    MovxA_dptr           ->
        case mval of
            Just v  -> c { acc = v, psw = (psw c) { psw_p = parityBit v } }
            Nothing -> c
    MovxA_ri _ri         ->
        case mval of
            Just v  -> c { acc = v, psw = (psw c) { psw_p = parityBit v } }
            Nothing -> c

    -- MOVX writes are handled by mcs51Write; no state change here
    MovxDptr_A  -> c
    MovxRi_A _  -> c

    -- MOVC: treated as simple NOP for now (requires code ROM access)
    MovcA_dptr -> c
    MovcA_pc   -> c

    PushDir dir ->
        let v   = readDirect c dir
            sp' = sp c + 1
            c'  = c { sp = sp' }
        in writeIram c' sp' v

    PopDir  dir ->
        let v   = readIram c (sp c)
            c'  = writeDirect c dir v
        in c' { sp = sp c - 1 }

    XchA_dir dir ->
        let v    = readDirect c dir
            old  = acc c
            c'   = writeDirect c dir old
        in c' { acc = v, psw = (psw c') { psw_p = parityBit v } }
    XchA_ri ri ->
        let addr = getReg c ri
            v    = readDirect c addr
            old  = acc c
            c'   = writeDirect c addr old
        in c' { acc = v, psw = (psw c') { psw_p = parityBit v } }
    XchA_rn rn ->
        let v   = getReg c rn
            old = acc c
        in (setReg c rn old) { acc = v, psw = (psw c) { psw_p = parityBit v } }
    XchdA_ri ri ->
        let addr = getReg c ri
            mem  = readDirect c addr
            newMem = (mem .&. 0xF0) .|. (acc c .&. 0x0F)
            newA   = (acc c .&. 0xF0) .|. (mem .&. 0x0F)
            c'     = writeDirect c addr newMem
        in c' { acc = newA, psw = (psw c') { psw_p = parityBit newA } }

    -- ── Boolean operations ───────────────────────────────────────────────────
    ClrC          -> c { psw = (psw c) { psw_cy = 0 } }
    SetbC         -> c { psw = (psw c) { psw_cy = 1 } }
    CplC          ->
        let cy' = unpack (complement (pack (psw_cy (psw c)) :: BitVector 1))
        in c { psw = (psw c) { psw_cy = cy' } }

    ClrBit ba     -> writeBit c ba 0
    SetbBit ba    -> writeBit c ba 1
    CplBit ba     -> writeBit c ba (complement1 (readBit c ba))

    AnlC_bit ba   ->
        let newCy = psw_cy (psw c) .&. readBit c ba
        in c { psw = (psw c) { psw_cy = newCy } }
    AnlC_nbit ba  ->
        let newCy = psw_cy (psw c) .&. complement1 (readBit c ba)
        in c { psw = (psw c) { psw_cy = newCy } }
    OrlC_bit ba   ->
        let newCy = psw_cy (psw c) .|. readBit c ba
        in c { psw = (psw c) { psw_cy = newCy } }
    OrlC_nbit ba  ->
        let newCy = psw_cy (psw c) .|. complement1 (readBit c ba)
        in c { psw = (psw c) { psw_cy = newCy } }

    MovC_bit ba   ->
        c { psw = (psw c) { psw_cy = readBit c ba } }
    MovBit_C ba   ->
        writeBit c ba (psw_cy (psw c))

    -- ── Branches (jump target computation deferred to mcs51Jump) ─────────────
    -- Modifying state for CJNE/DJNZ: update destination register
    DjnzRn  rn _  ->
        setReg c rn (getReg c rn - 1)
    DjnzDir dir _ ->
        let v = readDirect c dir - 1
        in writeDirect c dir v
    CjneA_imm imm rel ->
        let a  = acc c
            cy = if a < imm then 1 else 0
        in c { psw = (psw c) { psw_cy = cy } }
    CjneA_dir dir rel ->
        let a  = acc c
            v  = readDirect c dir
            cy = if a < v then 1 else 0
        in c { psw = (psw c) { psw_cy = cy } }
    CjneRi_imm ri imm rel ->
        let v  = readDirect c (getReg c ri)
            cy = if v < imm then 1 else 0
        in c { psw = (psw c) { psw_cy = cy } }
    CjneRn_imm rn imm rel ->
        let v  = getReg c rn
            cy = if v < imm then 1 else 0
        in c { psw = (psw c) { psw_cy = cy } }

    JbcBit ba _ ->
        writeBit c ba 0   -- clear the bit if branch taken (handled in jump)

    -- Remaining instructions don't modify state in compute
    _ -> c

-- | Determine the external XRAM write for MOVX store instructions.
mcs51Write :: Instruction -> CoreData -> Maybe (MCS51Addr, MCS51Word)
mcs51Write instr c = case instr of
    MovxDptr_A -> Just (getDptr c, acc c)
    MovxRi_A ri -> Just (zeroExtend (getReg c ri), acc c)
    _ -> Nothing

-- | Determine the next PC (Nothing = sequential, Just addr = jump).
--   Takes the POST-compute state so DJNZ/CJNE register updates are visible.
mcs51Jump :: Instruction -> MCS51Addr -> CoreData -> Maybe MCS51Addr
mcs51Jump instr seqPC c = case instr of
    LjmpAddr tgt   -> Just tgt
    AjmpAddr tgt   ->
        -- Upper 5 bits from seqPC, lower 11 from tgt
        Just ((seqPC .&. 0xF800) .|. (tgt .&. 0x07FF))
    SjmpRel  rel   -> Just (branchTarget seqPC rel)
    JmpAdptr       -> Just (getDptr c + zeroExtend (acc c))

    JzRel  rel     -> if acc c == 0           then Just (branchTarget seqPC rel) else Nothing
    JnzRel rel     -> if acc c /= 0           then Just (branchTarget seqPC rel) else Nothing
    JcRel  rel     -> if psw_cy (psw c) == 1  then Just (branchTarget seqPC rel) else Nothing
    JncRel rel     -> if psw_cy (psw c) == 0  then Just (branchTarget seqPC rel) else Nothing

    JbBit  ba rel  -> if readBit c ba == 1    then Just (branchTarget seqPC rel) else Nothing
    JnbBit ba rel  -> if readBit c ba == 0    then Just (branchTarget seqPC rel) else Nothing
    JbcBit ba rel  -> if readBit c ba == 1    then Just (branchTarget seqPC rel) else Nothing
                      -- Note: the bit has already been cleared by mcs51Compute.

    DjnzRn  rn  rel -> if getReg c rn /= 0   then Just (branchTarget seqPC rel) else Nothing
    DjnzDir dir rel ->
        let v = readDirect c dir
        in if v /= 0 then Just (branchTarget seqPC rel) else Nothing

    CjneA_imm imm rel ->
        if acc c /= imm                       then Just (branchTarget seqPC rel) else Nothing
    CjneA_dir dir rel ->
        if acc c /= readDirect c dir          then Just (branchTarget seqPC rel) else Nothing
    CjneRi_imm ri imm rel ->
        if readDirect c (getReg c ri) /= imm  then Just (branchTarget seqPC rel) else Nothing
    CjneRn_imm rn imm rel ->
        if getReg c rn /= imm                 then Just (branchTarget seqPC rel) else Nothing

    _ -> Nothing

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

complement1 :: Bit -> Bit
complement1 b = unpack (complement (pack b :: BitVector 1))
