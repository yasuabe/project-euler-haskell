-- TODO: too slow
import Data.Char
import Data.List

p = 8

sumd n = foldl' (\a b->a + (toInteger (digitToInt b)))0 $show n

isPowered m n = if 1==m || 0/=mod n m then False
                else if 1==n' then True else isPowered m n'
              where n' = div n m

e119 = dropWhile (<10)$sort$nub
     $ filter (\n->isPowered (sumd n) n)
                   $ concatMap (\n->takeWhile (<=10^(2*p))
                   $ map (\p->n^p)[2..])[2..10^p]

main = print$e119!!29 
