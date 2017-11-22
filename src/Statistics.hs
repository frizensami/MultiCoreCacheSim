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
                                   , getPrivateDataAccesses :: PrivateDataAccesses
                                   , getPublicDataAccesses  :: PublicDataAccesses 
                                   } deriving (Eq)

instance Show BusStatistics where
    show (BusStatistics bustraffic private public) = 
        "Bus Traffic (Bytes): " ++ (show bustraffic) ++ "\n" ++
        "Private Data Accesses: " ++ (show private) ++ "\n" ++ 
        "Public Data Accesses: " ++ (show public) ++ "\n"


addBusTrafficStats :: BusStatistics -> BusTrafficBytes -> BusStatistics
addBusTrafficStats (BusStatistics traffic private public) toAdd = BusStatistics (traffic + toAdd) private public

addBusPrivateAccessStats :: BusStatistics -> PrivateDataAccesses -> BusStatistics
addBusPrivateAccessStats (BusStatistics traffic private public) toAdd = BusStatistics traffic (private + toAdd) public

addBusPublicAccessStats :: BusStatistics -> PublicDataAccesses -> BusStatistics
addBusPublicAccessStats (BusStatistics traffic private public) toAdd = BusStatistics traffic private (public + toAdd)

incrementBusPrivateAccessStats :: BusStatistics -> BusStatistics
incrementBusPrivateAccessStats stats = addBusPrivateAccessStats stats 1

incrementBusPublicAccessStats :: BusStatistics -> BusStatistics
incrementBusPublicAccessStats  stats = addBusPublicAccessStats  stats 1



