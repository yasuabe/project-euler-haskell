import Data.Numbers.Primes
import Data.List

f 1 ps  = ps
f n ps@(h:t) | n==length ps = [product ps]
             | otherwise    = [h*m|m<-f (n-1) t] ++ f n t
g n ps@(h:t) = [(sum$map (div n)$f i ps)*(-1)^(i-1)|i<-[1..length ps]]
h n = n-(sum$g n ps) where ps=nub$primeFactors n

main = print$sum$map h [2..(10^6)]
