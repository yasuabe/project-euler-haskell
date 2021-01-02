import Data.Numbers.Primes
import Data.List
import Data.Bits

modPow b e m = modPow' e
  where
    modPow' 0 = 1
    modPow' e
      | e .&. 1 == 1 = ((b `mod` m) * modM) `mod` m
      | otherwise    = modM
      where
        modM = ((modPow' (shiftR e 1))^2) `mod` m
    
matched :: Int->Int->Bool
matched e p = modPow 10 e (9*p) == 1

main = print $ sum $ take 40 $ filter (matched (10^9)) primes
