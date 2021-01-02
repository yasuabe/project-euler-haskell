import Data.Numbers.Primes

f2 a1 b1 a2 b2 m
  | a1 == 1        = b1
  | 0 == mod b1 a1 && 1==gcd m a1 = div b1 a1
  | otherwise      = f2 a2 b2 (mod a1 a2) (mod (b1-b2*q) m) m
  where q = div a1 a2

h a b m
  | 0 == mod b a = div b a
  | otherwise    = div (m*x+b) a
        where x=f2 m (a-b) a 0 a

e381 p = mod (x3 + x4 + x5) p
  where x3 = h (p-2) 1  p
        x4 = h (p-3) x3 p
        x5 = h (p-4) x4 p

main =print$sum$map e381 $takeWhile (<10^8) $dropWhile (<5) primes
