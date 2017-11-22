module StatsSpec where

-- This test package uses HSpec
import Test.Hspec
import Statistics

main :: IO ()
main = hspec spec
    
spec :: Spec
spec = do
    describe "Simple processor statistics show output" $
        it "returns the expected string for 'let x = ProcessorStatistics 2 1000 50 940 10'" $
            let x = ProcessorStatistics 1000 50 940 10 2 in  
                show x `shouldBe` 
                    "PID: 2 | Compute Cycles: 1000 | Load Store Instructions: 50 | Idle Cycles: 940 | Cache Miss Rate: 20.0%"

    describe "Simple simulation statistics show output" $
        it "returns the expected string for 'let x = ProcessorStatistics 2 1000 50 940 10', 'let y = SimulationStatistics 5000 [x] 7777 30 10`, show y" $
            let 
                x = ProcessorStatistics 1000 50 940 10 2 
                y = SimulationStatistics 5000 [x] (BusStatistics 7777 0 30 10)
                in  
                show y `shouldBe` 
                    "\n-----SIMULATION STATISTICS REPORT-----\nTotal Cycles: 5000\n\n--------------------------------------------\nPID: 2 | Compute Cycles: 1000 | Load Store Instructions: 50 | Idle Cycles: 940 | Cache Miss Rate: 20.0%\n--------------------------------------------\n\nBus Traffic (Bytes): 7777\nBus invalidations/updates: 0\nPrivate Data Accesses: 30\nPublic Data Accesses: 10\n"
                    
                    

   

