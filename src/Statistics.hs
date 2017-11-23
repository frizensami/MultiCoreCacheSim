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
                                                 , getCacheStatistics     :: CacheStatistics
                                                 } deriving (Eq)
                                                 

instance Show SimulationStatistics where
    show (SimulationStatistics cycles processor_xs busstats cachestats) = 
        "\n-----SIMULATION STATISTICS REPORT-----\n" ++
        "Total Cycles: " ++ (show cycles) ++ "\n" ++
        (concatMap (("\n--------------------------------------------\n" ++) . show) processor_xs) ++
        "\n--------------------------------------------\n\n" ++
        show busstats ++ "\n" ++
        show cachestats


data BusStatistics = BusStatistics { getBusTrafficBytes     :: BusTrafficBytes
                                   , busInvalidationsUpdates:: Int
                                   } deriving (Eq)

instance Show BusStatistics where
    show (BusStatistics bustraffic ivupd) = 
        "Bus Traffic (Bytes): " ++ (show bustraffic) ++ "\n" ++
        "Bus invalidations/updates: " ++ (show ivupd) ++ "\n"

createBusStatistics :: BusStatistics
createBusStatistics = BusStatistics 0 0

addBusTrafficStats :: BusTrafficBytes -> BusStatistics -> BusStatistics
addBusTrafficStats toAdd (BusStatistics traffic ivupd) = BusStatistics (traffic + toAdd) ivupd

addBusIvUpdStats :: BusTrafficBytes -> BusStatistics -> BusStatistics
addBusIvUpdStats toAdd (BusStatistics traffic ivupd) = BusStatistics traffic (ivupd + toAdd)

incrementBusIvUpdStats :: BusStatistics -> BusStatistics
incrementBusIvUpdStats  stats = addBusIvUpdStats 1 stats

data CacheStatistics = CacheStatistics { getPrivateDataAccesses :: PrivateDataAccesses
                                       , getPublicDataAccesses :: PublicDataAccesses
                                       } deriving (Eq)

instance Show CacheStatistics where
    show (CacheStatistics private public) =
        "Cache private accesses: " ++ (show private) ++ "\n" ++
        "Cache public accesses: " ++ (show public) ++ "\n"

createCacheStatistics :: CacheStatistics
createCacheStatistics = CacheStatistics 0 0

addCachePrivateAccessStats :: PrivateDataAccesses -> CacheStatistics -> CacheStatistics
addCachePrivateAccessStats toAdd (CacheStatistics private public) = CacheStatistics (private + toAdd) public

addCachePublicAccessStats :: PublicDataAccesses -> CacheStatistics -> CacheStatistics
addCachePublicAccessStats toAdd (CacheStatistics private public) = CacheStatistics private (public + toAdd)

incrementCachePrivateAccessStats :: CacheStatistics -> CacheStatistics
incrementCachePrivateAccessStats stats = addCachePrivateAccessStats 1 stats

incrementCachePublicAccessStats :: CacheStatistics -> CacheStatistics
incrementCachePublicAccessStats  stats = addCachePublicAccessStats 1 stats
