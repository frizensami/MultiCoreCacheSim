-- This test package uses HSpec
import Bus
import Test.Hspec
import Data.Sequence
import Statistics

main :: IO ()
main = hspec $ do

    -- BUS TESTS

    describe "Bus.isBusEmpty" $ do
        it "returns true for an empty bus" $ do
            isBusEmpty (EventBus (empty)) `shouldBe` True

    describe "Bus.createBus" $ do
        it "creates an empty bus" $ do  
            isBusEmpty createBus `shouldBe` True

    describe "Bus.enqueueBus" $ do
        it "causes bus to be non-empty" $ do  
            isBusEmpty (enqueueBus createBus "asdf") `shouldBe` False
            
    describe "Bus.dequeueBus on Bus.enqueueBus" $ do
        it "returns the enqueued element and the empty bus" $ do  
            (dequeueBus (enqueueBus createBus "asdf")) `shouldBe` (createBus, "asdf")

    describe "Bus.dequeueBus on Bus.enqueueBus twice" $ do
        it "returns the last enqueued element and the empty bus" $ do  
            (dequeueBus $ enqueueBus (enqueueBus createBus "asdf") "last") `shouldBe` ((enqueueBus createBus "asdf"), "last")
    
    -- STATS TESTS

    describe "Simple processor statistics show output" $ do
        it "returns the expected string for 'let x = ProcessorStatistics 2 1000 50 940 10'" $ do
            let x = ProcessorStatistics 2 1000 50 940 10  in  
                (show x) `shouldBe` 
                    "PID: 2 | Compute Cycles: 1000 | Load Store Instructions: 50 | Idle Cycles: 940 | Cache Miss Rate: 0.2."

    describe "Simple simulation statistics show output" $ do
        it "returns the expected string for 'let x = ProcessorStatistics 2 1000 50 940 10', 'let y = SimulationStatistics 5000 [x] 7777 30 10`, show y" $ do
            let 
                x = ProcessorStatistics 2 1000 50 940 10 
                y = SimulationStatistics 5000 [x] 7777 30 10
                in  
                (show y) `shouldBe` 
                    "\n-----SIMULATION STATISTICS REPORT-----\nTotal Cycles: 5000\n\n--------------------------------------------\nPID: 2 | Compute Cycles: 1000 | Load Store Instructions: 50 | Idle Cycles: 940 | Cache Miss Rate: 0.2.\n--------------------------------------------\n\nBus Traffic (Bytes): 7777\nPrivate Data Accesses: 30\nPublic Data Accesses: 10\n"
                    
                    

   

