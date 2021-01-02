import Data.Numbers.Primes

e n = e' 2
  where
    l = (floor.sqrt.fromIntegral) n
    e' m
      | l < m      = True
      | 0/=mod n m = e' (m+1)
      | isPrime'   = e' (m+1)
      | otherwise  = False 
      where isPrime' = let q=div n m in isPrime (q+m)

main = print$sum$filter e$map (\p->p-1)$takeWhile(<10^8)$primes
