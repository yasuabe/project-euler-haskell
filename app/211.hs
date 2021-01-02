-- TODO: too slow
import Data.Numbers.Primes

isSquare n = n==((floor.sqrt.fromIntegral) n)^2
e211 1 = 1
e211 n = f 4 fh 1 ft 
  where
    (fh:ft) = primeFactors n
    f ex pr sm [] = sm * (div (pr^ex-1) (pr^2-1))
    f ex pr sm ns@(nh:nt)
      | pr == nh  = f (ex+2) pr sm nt
      | otherwise = f 4 nh (sm * (div (pr^ex-1) (pr^2-1))) nt
main = print$sum$filter (\n->isSquare $e211 n) [1..64000000]
