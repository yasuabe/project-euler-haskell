--need better algorithm
import Data.Numbers.Primes
import Data.Function
import Data.List

f candLists prm=f ++ s where (f,s)=foldr (\a (b,c)->if f1 a prm then (((prm:a):b),(a:c)) else (b,(a:c))) ([],[[prm]]) candLists
f1 cands prm= all (\cand->f2 cand prm) cands
f2 cand  prm= ((&&) `on` isPrime) v1 v2 where
    v1 = cand*10^lp+prm
    v2 = prm*10^lc+cand
    lc=width cand
    lp=width prm
    width =length.show
--main =print$find (\(a,b)->b==4)$map (\l->(l,length l))$foldl' f [] $take 2000$3:(drop 3 primes)
main = print
     $ sum $ fst
     $ filter (\(_,l)-> 4<=l)
     $ map (\l-> (l,length l))
     $ foldl' f []
     $ take 2000
     $ 3:drop 3 primes

-- $ ./60 +RTS -K500M -RTS
-- ([8389,6733,5701,5197,13],5)
--
