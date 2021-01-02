--need refactor
--need better algorithm
--need performance enhancement
import Data.Numbers.Primes
import Data.List
import Data.Function

main =print$product.fst$e70
e70 =minimumBy (\a b->(on compare snd)a b)$map (\ps->(ps,g ps))$filter (\n->(sort$show$product n)==(sort$show$h n))$filter (\l->(0<length l)&&(all (==1)$map length$group l))$map primeFactors [1..10^7]
h ps= (\m->div (m*n) (product ps))$foldr (\p b->(p-1)*b) 1 ps where n = product ps
g ps=(\(n,d)->(fromIntegral n)/(fromIntegral d))$foldr (\(n,d) (nr,dr)->(n*nr,d*dr)) (1,1) $map (\p->(p,p-1)) ps
