import Data.List
import Data.Char

p17 = filter'$map show'$takeWhile (<1000)$map (*17) [1..] where
    filter' = filter (\s->s == nub s)
    show' n = if len < 3 then (take (3 - len)$ repeat '0')++n' else n'
              where n' = show n; len = length n'

e43 ps [] = map (\s->remindar s++s) ps
e43 ps (d:ds) = e43 [x:p|p<-ps, x<-remindar p, divisable x p d] ds
    where divisable x p d = 0==mod (read(x:take 2 p)) d

remindar s = ['0'..'9']\\s

main = print$sum$ map read $e43 p17 [13,11,7,5,3,2]
--ghci> :main
--16695334890

