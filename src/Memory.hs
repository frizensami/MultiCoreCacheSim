module Memory (Memory, create, read, write, elapse, getReadAddress, getWrittenAddress) where

import Definitions
import Prelude hiding (read)

-- Constants describing number of cycles for read/write operation
readCycles = 100
writeCycles = 100

data Memory = Memory {
    getBusyCycles :: NumCycles,
    getReadingAddress :: Maybe MemoryAddress,
    getWritingAddress :: Maybe MemoryAddress
} deriving (Show)

main = print $ Memory.elapse 10 $ snd $ Memory.write 0x00000001 $ Memory.create

create :: Memory
create = Memory busyCycles readingAddress writingAddress where
    busyCycles = 0
    readingAddress = Nothing
    writingAddress = Nothing

read :: MemoryAddress -> Memory -> (IsBusy, Memory)
read memoryAddress memory = (isBusy, newMemory) where
    isBusy = currentBusyCycles /= 0 where
        currentBusyCycles = getBusyCycles memory

    newMemory =
        if isBusy
            then memory
            else Memory readCycles newReadingAddress oldWritingAddress where
                newReadingAddress = Just memoryAddress
                oldWritingAddress = getWritingAddress memory

write :: MemoryAddress -> Memory -> (IsBusy, Memory)
write memoryAddress memory = (isBusy, newMemory) where
    isBusy = currentBusyCycles /= 0 where
        currentBusyCycles = getBusyCycles memory

    newMemory =
        if isBusy
            then memory
            else Memory writeCycles oldReadingAddress newWritingAddress where
                oldReadingAddress = getReadingAddress memory
                newWritingAddress = Just memoryAddress

elapse :: NumCycles -> Memory -> (IsBusy, Memory)
elapse numCycles memory = (isBusy, newMemory) where
    -- Update the busy cycles while still clamping it to 0 --
    newBusyCycles = max (oldBusyCycles - numCycles) 0 where
        oldBusyCycles = getBusyCycles memory

    isBusy = newBusyCycles /= 0
    newMemory = Memory newBusyCycles oldReadingAddress oldWritingAddress where
        oldReadingAddress = getReadingAddress memory
        oldWritingAddress = getWritingAddress memory

getReadAddress :: Memory -> Maybe MemoryAddress
getReadAddress memory =
    if getBusyCycles memory == 0
        then getReadingAddress memory
        else Nothing

getWrittenAddress :: Memory -> Maybe MemoryAddress
getWrittenAddress memory =
    if getBusyCycles memory == 0
        then getWritingAddress memory
        else Nothing
