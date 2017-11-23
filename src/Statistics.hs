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
        "Cache Miss Rate: " ++ (show ((fromIntegral misscount / fromIntegral loadstore) * 100 :: Double)) ++ "%"

newProcessorStatistics :: Int -> ProcessorStatistics
newProcessorStatistics = ProcessorStatistics 0 0 0 0

type TotalCycles = Int
type BusTrafficBytes = Int
type PrivateDataAccesses = Int
type PublicDataAccesses = Int

data SimulationStatistics = SimulationStatistics { getTotalCycles         :: TotalCycles
                                                 , getProcessorStats      :: [ProcessorStatistics]
                                                 , getBusStatistics       :: BusStatistics
                                                 } deriving (Eq)
                                                 

instance Show SimulationStatistics where
    show (SimulationStatistics cycles processor_xs busstats) = 
        "\n-----SIMULATION STATISTICS REPORT-----\n" ++
        "Total Cycles: " ++ (show cycles) ++ "\n" ++
        (concatMap (("\n--------------------------------------------\n" ++) . show) processor_xs) ++
        "\n--------------------------------------------\n\n" ++
        show busstats


data BusStatistics = BusStatistics { getBusTrafficBytes     :: BusTrafficBytes
                                   , busInvalidationsUpdates:: Int
                                   , getPrivateDataAccesses :: PrivateDataAccesses
                                   , getPublicDataAccesses  :: PublicDataAccesses 
                                   } deriving (Eq)

instance Show BusStatistics where
    show (BusStatistics bustraffic ivupd private public) = 
        "Bus Traffic (Bytes): " ++ (show bustraffic) ++ "\n" ++
        "Bus invalidations/updates: " ++ (show ivupd) ++ "\n" ++
        "Private Data Accesses: " ++ (show private) ++ "\n" ++ 
        "Public Data Accesses: " ++ (show public) ++ "\n"

newBusStatistics :: BusStatistics
newBusStatistics = BusStatistics 0 0 0 0

addBusTrafficStats :: BusTrafficBytes -> BusStatistics -> BusStatistics
addBusTrafficStats toAdd (BusStatistics traffic ivupd private public) = BusStatistics (traffic + toAdd) ivupd private public

addBusIvUpdStats :: BusTrafficBytes -> BusStatistics -> BusStatistics
addBusIvUpdStats toAdd (BusStatistics traffic ivupd private public) = BusStatistics traffic (ivupd + toAdd) private public

addBusPrivateAccessStats :: PrivateDataAccesses -> BusStatistics -> BusStatistics
addBusPrivateAccessStats toAdd (BusStatistics traffic ivupd private public) = BusStatistics traffic ivupd (private + toAdd) public

addBusPublicAccessStats :: PublicDataAccesses -> BusStatistics -> BusStatistics
addBusPublicAccessStats toAdd (BusStatistics traffic ivupd private public) = BusStatistics traffic ivupd private (public + toAdd)

incrementBusPrivateAccessStats :: BusStatistics -> BusStatistics
incrementBusPrivateAccessStats stats = addBusPrivateAccessStats 1 stats

incrementBusPublicAccessStats :: BusStatistics -> BusStatistics
incrementBusPublicAccessStats  stats = addBusPublicAccessStats 1 stats

incrementBusIvUpdStats :: BusStatistics -> BusStatistics
incrementBusIvUpdStats  stats = addBusIvUpdStats 1 stats
