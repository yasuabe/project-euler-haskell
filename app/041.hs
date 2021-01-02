import Data.Numbers.Primes
import Data.Char
import Data.List

main = print$pe041 9
pe041 :: Int->Int
pe041 n = if null cand then pe041 (n-1) else read $maximum cand
    where cand = filter (isPrime.read)$permutations ['1'..(intToDigit n)]

--ghci> :main
--7652413
--
