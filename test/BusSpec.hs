module BusSpec where

-- This test package uses HSpec
import Bus
import Test.Hspec
import Data.Sequence

main :: IO ()
main = hspec spec
    
spec :: Spec
spec = do
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
                    

   

