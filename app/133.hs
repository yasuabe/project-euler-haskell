import Data.Numbers.Primes
import Data.List

modPow b e m = modPow' e
  where
    modPow' 0 = 1
    modPow' e
      | r == 1    = ((b `mod` m) * modM) `mod` m
      | otherwise = modM
      where
        (q, r) = e `divMod` 2
        modM = ((modPow' q)^2) `mod` m
    
matched :: Integer->Integer->Bool
matched e p = modPow 10 e (9*p) /= 1

solve133 n = sum $ filter (matched (10^n)) $ takeWhile (<100000) $ primes

main = print $ solve133 16
