import Data.List
import qualified Data.Set as S
import Data.Char

sof ds = sum$map (prod.digitToInt) ds where prod n = product[1..n]
f ds chain = if elem ds' chain' then chain' else f ds' chain'
    where ds'    = reverse$sort$show$sof ds
          chain' = ds:chain

h w = map reverse$drop 1$h' w False '0' where
    h' 0 _ _  = [[]]
    h' w z ll = [d:ss|d<-[ll..'9'],ss<-h' (w-1) True d]

e74= length$filter ((/='0').head)
   $ nub$concatMap permutations
   $ filter (\l->(==60)$length$f l [])
   $ concatMap h [1..6]
main=print$e74
