import Data.Numbers.Primes
import Data.List

f prod i
  | 10^9 < prod = []
  | otherwise   = (g prod (primes!!(i+1))) : (adms i) ++ (adms (i+1))
  where  adms k = f (prod * primes!!k) k
         g a p  | isPrime (a+p) = p
                | otherwise     = g a (p+2)

main = print $ sum $ nub $ f 2 0
