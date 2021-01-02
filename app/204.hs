import Data.Numbers.Primes
f p a | (10^9)<a      = 0
      | otherwise = 1 + (g p a)
g p a = sum$filter (0<)$map (\q->f q (a*q)) $filter (p<=) $takeWhile (<100) primes
main = print $f 1 1
