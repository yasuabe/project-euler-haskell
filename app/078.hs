import Control.Monad
import Data.Array.IO

pentagonal n = div (n*(3*n-1)) 2
generalised_pentagonal n
    | n < 0        = 0
    | 0 == mod n 2 = pentagonal (div n 2+1)
    | otherwise    = pentagonal (-(div n 2+1))

getR :: Integer->Integer->Integer->Integer->IOArray Integer Integer->IO Integer
getR n r f i arr
    | n < k     = return r
    | otherwise = do let f' = if 0==mod i 2 then (-f) else f
                     r' <- readArray arr (n-k)
                     getR n (r+(f'*r')) f' (i+1) arr
    where k = generalised_pentagonal i

getPartitionNumbers lmt = do
    arr<-newArray (0,lmt+1) 0::IO (IOArray Integer Integer)
    writeArray arr 0 1
    r <- getPartitionNumbers' 1 lmt arr
    print r

getPartitionNumbers' n lmt arr = do
    r' <- r
    if 0 == mod r' 1000000 then return n
                           else do writeArray arr n r'  
                                   getPartitionNumbers' (n+1) lmt arr   
    where r = getR n 0 (-1) 0 arr

main = getPartitionNumbers 1000000
