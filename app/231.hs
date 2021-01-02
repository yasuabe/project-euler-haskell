import Data.List
import Data.Numbers.Primes

pe231 = (f 15000001 20000000) - (f 1 5000000) 
  where f s e = foldl' (\s n->s + ((sum.primeFactors) n)) 0 [s..e]
main = print pe231
