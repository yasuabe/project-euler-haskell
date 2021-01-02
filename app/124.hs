import Data.Numbers.Primes
import Data.List

rad n = product$map head$group$primeFactors n
compareRads (n1,r1) (n2,r2) = let c = compare r1 r2 in
    if EQ==c then compare n1 n2 else c

main = print $ l!!9999
    where l = sortBy compareRads$map (\n->(n,rad n)) [1..100000]
