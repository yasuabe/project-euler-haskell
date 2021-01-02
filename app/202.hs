import Data.Numbers.Primes
import Data.List
f bounces = (((a-1)`div`3) -) 
          $ sum 
          $ map (\(b, n) -> b*(1 + (a - (mod b 3)*n)`div`(3*n))) 
          $ map (\ps->(-(-1)^(length ps), product ps))
          $ drop 1 
          $ subsequences 
          $ nub 
          $ primeFactors a where a=(bounces+3)`div`2

main = print $ f 12017639147
