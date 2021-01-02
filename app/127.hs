-- TODO: too slow
import Data.Numbers.Primes
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

f 0 b res = res 
f a b res
    | gcd a b == 1 && gcd a c == 1 && gcd b c == 1 && rad (a*b*c) < c = f (a-1)(b+1) ((a,b,c):res)
    | otherwise = f (a-1) (b+1) res
    where c = a + b
rad x = product$nub$primeFactors x
g n = f (n - (div n 2)) (div n 2) []

--main = print$foldr (\(a,b,c) s->s+c) 0$concat$map g [2..119999]
main = print$sum$map(\(a,b)->a+b)$concat$map m6 [2..119999]


dpf :: Integral int => int -> S.Set int
dpf n = factors n (wheelSieve 6)
 where
  factors 1 _                  = S.empty
  factors m (p:ps) | m < p*p   = S.singleton m
                   | r == 0    = S.insert p $ factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

m = M.fromList $map (\n->(n,dpf n)) [1..119999]
m2 n = M.filterWithKey (\k s->k<n&&(product (S.toList s))<r&&S.empty==(S.intersection s (dpf n))) m where r = div n (rad n)
m3 n = M.filterWithKey (\k s->k < (div n 2)) (m2 n)
m4 n = M.filterWithKey (\k _->M.member (n-k) (m2 n)) (m3 n)
m5 n = M.mapWithKey (\k v ->(S.toList v)++(S.toList ((M.!) (m2 n) (n-k)))) (m4 n)
m6 n = map (\(k,v)->(k,n-k))$filter (\(k,v)->(product v)<r)$ M.toList $m5 n where r = div n (rad n)

-- $ ghc 127.hs -O -rtsopts=all
-- $ ./127 +RTS -K500M -RTS
