module Memory (create, read, write, elapse) where

import Definitions
import Prelude hiding (read)

-- Constants describing number of cycles for read/write operation
readCycles = 100
writeCycles = 100

main = print $ Memory.elapse 10 $ snd $ Memory.write 0x00000001 $ Memory.create

create :: Memory
create = Memory busyCycles readingAddress writingAddress where 
    busyCycles = 0
    readingAddress = Nothing
    writingAddress = Nothing

read :: MemoryAddress -> Memory -> (IsBusy, Memory)
read memoryAddress memory = (isBusy, newMemory) where 
    isBusy = (busyCycles memory) /= 0
    newMemory = 
        if isBusy 
            then memory
            else Memory readCycles (Just memoryAddress) (writingAddress memory)

write :: MemoryAddress -> Memory -> (IsBusy, Memory)
write memoryAddress memory = (isBusy, newMemory) where 
    isBusy = (busyCycles memory) /= 0
    newMemory = 
        if isBusy 
            then memory
            else Memory writeCycles (readingAddress memory) (Just memoryAddress)

elapse :: NumCycles -> Memory -> (IsBusy, Memory)
elapse numCycles memory = (isBusy, newMemory) where 
    -- Update the busy cycles while still clamping it to 0 --
    newBusyCycles = max (busyCycles memory - numCycles) 0
    isBusy = newBusyCycles /= 0
    newMemory = Memory newBusyCycles (readingAddress memory) (writingAddress memory)
