module Statistics where

type ProcessorID = Int
type ComputeCycles = Int
type LoadStoreCount = Int
type IdleCycles = Int
type CacheMissCount = Int

data ProcessorStatistics = ProcessorStatistics { getComputeCycles  :: ComputeCycles
                                               , getLoadStoreCount :: LoadStoreCount
                                               , getNumIdleCycles  :: IdleCycles
                                               , getCacheMissCount :: CacheMissCount 
                                               , getProcessorID    :: ProcessorID
                                               } deriving (Eq)

instance Show ProcessorStatistics where
    show (ProcessorStatistics compute loadstore idle misscount pid) = 
        "PID: " ++ (show pid) ++ " | " ++
        "Compute Cycles: " ++ (show compute) ++ " | " ++
        "Load Store Instructions: " ++ (show loadstore) ++ " | " ++ 
        "Idle Cycles: " ++ (show idle) ++ " | " ++
        "Cache Miss Rate: " ++ (show ((fromIntegral misscount / fromIntegral loadstore) :: Double)) ++ "."

newProcessorStatistics :: Int -> ProcessorStatistics
newProcessorStatistics processorID = ProcessorStatistics 0 0 0 0 processorID

type TotalCycles = Int
type BusTrafficBytes = Int
type PrivateDataAccesses = Int
type PublicDataAccesses = Int

data SimulationStatistics = SimulationStatistics { getTotalCycles         :: TotalCycles
                                                 , getProcessorStats      :: [ProcessorStatistics]
                                                 , getBusTrafficBytes     :: BusTrafficBytes
                                                 , getPrivateDataAccesses :: PrivateDataAccesses
                                                 , getPublicDataAccesses  :: PublicDataAccesses
                                                 } deriving (Eq)
                                                 

instance Show SimulationStatistics where
    show (SimulationStatistics cycles processor_xs bustraffic private public) = 
        "\n-----SIMULATION STATISTICS REPORT-----\n" ++
        "Total Cycles: " ++ (show cycles) ++ "\n" ++
        (concatMap (("\n--------------------------------------------\n" ++) . show) processor_xs) ++
        "\n--------------------------------------------\n\n" ++
        "Bus Traffic (Bytes): " ++ (show bustraffic) ++ "\n" ++
        "Private Data Accesses: " ++ (show private) ++ "\n" ++ 
        "Public Data Accesses: " ++ (show public) ++ "\n"



