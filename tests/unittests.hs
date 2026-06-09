import Prelude

import Test.Tasty

import qualified Tests.Core.Harvard.ISA
import qualified Tests.Core.Harvard.Pipeline
import qualified Tests.Core.GPIO
import qualified Tests.Example.Project
import qualified Tests.MCS51.InstructionSet
import qualified Tests.MCS51.Instructions
import qualified Tests.MCS51.Interrupt
import qualified Tests.MCS51.CPU

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Core.Harvard.ISA.isaTests
  , Tests.Core.Harvard.Pipeline.pipelineTests
  , Tests.Core.GPIO.gpioTests
  , Tests.Example.Project.accumTests
  , Tests.MCS51.InstructionSet.instrTests
  , Tests.MCS51.Instructions.instructionTests
  , Tests.MCS51.Interrupt.interruptTests
  , Tests.MCS51.CPU.cpuTests
  ]
