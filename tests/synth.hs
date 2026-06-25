{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Prelude
import System.Directory (createDirectoryIfMissing)

import Hdl.Types
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..), execDesign)
import Hdl.Emit.Vhdl
import Isacle.ISA.Backend.SynthCPU (synthHarvardCPU)

import MCS51.ISA (mcs51ISA)
import MCS51.ISA.Types (mcs51CPUDef)

data Sys

instance KnownDom Sys where
    domId _ = DomId "sys" 12000000 Rising ActiveHigh "rst"

main :: IO ()
main = do
    let outDir = "build/mcs51_cpu"
    createDirectoryIfMissing True outDir
    let design = execDesign "mcs51_cpu" $
            synthHarvardCPU @Sys @8 @8 @8 @16 mcs51CPUDef mcs51ISA
    emitVhdlDesignFiles outDir design
    putStrLn $ "MCS-51 synthesis done — VHDL written to " ++ outDir
