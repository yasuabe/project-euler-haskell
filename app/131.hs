import Data.Numbers.Primes

p131 n = length
     $filter (\(p,a)->p ==((a+1)^3-a^3))
     $map (\p->(p,floor $sqrt (fromIntegral p/ 3.0)))
     $takeWhile (<10^n)$primes
main = print$p131 6
