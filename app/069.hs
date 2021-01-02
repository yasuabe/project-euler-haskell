import Data.Numbers.Primes

f result (prm:prms)
    | 1000000 < result' = result
    | otherwise      = f result' prms
    where result' = result * prm
main = print $ f 1 primes
