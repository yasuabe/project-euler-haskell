import Data.Numbers.Primes
import Data.List
f1 n = if g k1 < g k2 then  k2 else k1
    where n' = fromIntegral n
          k  = n'/(exp 1)
          k1 = floor k
          k2 = ceiling k
          g k2 = let k'=fromIntegral k2 in k'*log (n'/k')
f2 n = if null$(nub$(primeFactors k\\primeFactors n))\\[2,5]
       then -n else n
    where k = f1 n
e183 = print$sum$map f2 [5..10000]

e183' = print$sum$map f3 [5..10000] where
    f3 n = if null$(nub$(primeFactors k\\primeFactors n))\\[2,5]
           then -n else n
           where k = round$(fromIntegral n)/(exp 1)
--ghci> e183
