import Data.Function
import Data.Numbers.Primes
import Data.Array
import Data.Array.ST
import Data.List

u = 10^11
v = fromInteger $ div u (5^3 * 13^2)
w = fromInteger $ div u (5^3 * 13^2 * 17)

primes1 = filter (\p->1==mod p 4) $ takeWhile (<=v) primes 

multi = mkArray $ scanl1 (+) $ elems $ runSTArray 
      $ do arr <- newArray_ (0, w)
           mapM (\i -> writeArray arr i (toInteger i)) [0..w]
           mapM (\p -> mapM (\i->writeArray arr i 0) [p,2*p..w]) primes1
           return arr
      where mkArray arr = listArray (0, length arr) arr

bdr d ed = floor $ (fromIntegral $ div u d)**(1/ed)

sum1 cur = foldr fq cur rs where
  rs = takeWhile (<=bdr (13*5^2) (1/3))$ primes1
  fq r cur   = foldr fp cur qs where
    qs = filter (/=r) $ takeWhile (<=bdr (5*r^3) (1/2)) primes1
    fp q cur = foldr add cur ps where 
      ps = filter (\p-> p/=q && p/=r) $ takeWhile (<=bdr (q^2*r^3) 1) primes1
      add p cur = cur + n*m where
        n = p * q^2 * r^3
        m = multi!(fromInteger $div u n)

sum2 cur = foldr fq cur rs where
  rs = takeWhile (<=bdr (5^3) (1/7)) primes1
  fq r cur = foldr add cur qs where
    qs = filter (/=r) $ takeWhile (<=bdr (r^7) (1/3)) primes1
    add q cur = cur + n*m where
      n = q^3 * r^7
      m = multi!(fromInteger $div u n)

sum3 cur = foldr fq cur rs where
  rs = takeWhile (<= bdr (5^2) (1/10)) primes1
  fq r cur = foldr add cur qs where
    qs = filter (/=r) $ takeWhile (<=bdr (r^7) (1/2)) primes1
    add q cur = cur + n*m where
      n = q^2 * r^10
      m = multi!(fromInteger $div u n)

main = print $ sum3 $ sum2 $ sum1 0
