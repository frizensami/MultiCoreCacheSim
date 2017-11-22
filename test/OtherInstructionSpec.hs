module OtherInstructionSpec where

-- This test package uses HSpec
import Test.Hspec
import SimulatorCore
import Processor
import Statistics
import Trace
import Memory
import qualified Bus

main :: IO ()
main = hspec spec
    
spec :: Spec
spec =
    describe "SimulatorCore.runAllSimulationCycles" $
        context "when given one processor and a list of traces with only OtherInstructions" $
            it "Returns a statistics report that counts those OtherInstruction cycles correctly" $
                let 
                    newProcessorWithoutID = Processor.createProcessor "MESI" 64 1 64
                    processorsList = map newProcessorWithoutID [0..(num_processors-1)] 
                    tracesList = replicate 4 [OtherInstruction 1, OtherInstruction 5, OtherInstruction 10]
                    processorTraceList = zip processorsList tracesList
                    expectedTotal = 16
                    eventBus = Bus.create (map getCache processorsList) Memory.create

                    -- Run to get result
                    statsReport = runAllSimulationCycles processorTraceList eventBus 0 0

                    -- Expected statistics output
                    procExpectedStats = map (ProcessorStatistics expectedTotal 0 0 0) [0..(num_processors-1)]
                    simExpectedStats = SimulationStatistics expectedTotal procExpectedStats (BusStatistics 0 0 0 0)
                    in
                        statsReport `shouldBe` simExpectedStats

       
                    

   

