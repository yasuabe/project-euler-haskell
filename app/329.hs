import Data.Numbers.Primes
import Data.Ratio

f r p (c:cs) 
  | null cs   = r * ratio
  | p == 1    = f (r*ratio) 2   cs
  | p == 500  = f (r*ratio) 499 cs
  | otherwise = r*ratio*((f (1%2) (p-1) cs) + (f (1%2) (p+1) cs))
  where ratio | isPrime p     && c=='P' = 2%3
              | isPrime p     && c=='N' = 1%3
              | not(isPrime p)&& c=='P' = 1%3
              | not(isPrime p)&& c=='N' = 2%3

main = print $ sum $ map (\n->f (1%500) n "PPPPNNPPPNPPNPN") [1..500]
