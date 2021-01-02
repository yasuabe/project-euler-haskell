import Control.Monad
import Data.Array.IO
import Data.Numbers.Primes

f :: Int->Int->IOArray (Int,Int) Int->IO Int
f 0 _ _   = return 1
f _ 0 _   = return 1
f m n arr
    | m<n       = f m m arr
    | otherwise = do
        v'' <- readArray arr (m,n)
        if 0< v'' then return v'' else v'
    where v' = do
           v'''<- foldM (\a b ->do {b'<-b; return $a+b'}) 0
                $ map (\k->f (m-k) k arr)$takeWhile (<=n) primes 
           writeArray arr (m,n) v'''
           return v'''

main =do 
    arr <- newArray ((1,1),(100,100)) 0::IO (IOArray (Int,Int) Int)
    result <- f 100 99 arr
    print result
--ghci> f 100 99 arr
--40899
--ghci> f 50 49 arr
--819
--ghci> f 75 74 arr
--6845
--ghci> f 63 62 arr
--2605
--ghci> f 69 68 arr
--4268
--ghci> f 71 70 arr
--5006
--
