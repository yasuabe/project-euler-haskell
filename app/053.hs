import Data.List
import Data.Maybe

main = print
     $ sum
     $ map (\(n,r)->n-2*r+1)
     $ map (fst.fromJust)
     $ filter isJust
     $ map f [1..100]
     where f n = find ((10^6<).snd)
               $ map (\r->((n,r), div (product [(r+1)..n]) (product [1..(n-r)]))) [1..n]
