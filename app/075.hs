import Control.Monad
import Data.Array.IO
import Data.Array

countPrimitive l = length $ g2 l range where
    range = (ceiling ((sqrt' l)/2), (ceiling.sqrt'$ div l 2)-1)
    sqrt' = sqrt.fromIntegral
    g2 l (s,e) = do
        m <- [s..e]
        let l'= div l 2
        guard $ 0 == mod l' m
        let n = -m+(div l' m)
        guard $ 1 == gcd (m^2+n^2) (2*m*n)
        return m

countTriangles limit a = mapM_ update [12,14..limit] where
    update i = do 
        let cnt = countPrimitive i
        mapM_ (\idx->addCount idx cnt) [i,i+i..limit]
    addCount idx cnt = do
        v <- readArray a idx
        writeArray a idx (v+cnt)

e75 limit = do
    a  <- newArray (1,limit) 0::IO (IOArray Int Int)
    countTriangles limit a
    a''<- freeze a
    return $length$filter (1==)$elems a''

main = do e75 1500000 >>= print
