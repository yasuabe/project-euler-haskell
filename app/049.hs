import Data.Numbers.Primes
import Data.List

primes49 = dropWhile(<1000)$takeWhile(<10000)$primes

e49 (p1:[]) = []
e49 (p1:ps) = [ (p1, pn, pn-p1+pn) |
                 pn<-(ps), elem (pn-p1+pn) ps, equiv p1 pn (pn-p1+pn) 
              ] ++ e49 ps
equiv n m l = c n == c m && c m == c l where c = sort.show

main = do
         let [_, (a, b, c)] = e49 primes49
         print $ concatMap show [a, b, c]
