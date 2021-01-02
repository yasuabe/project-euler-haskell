import Data.List
import Data.Numbers.Primes

main = print $ sum $ map snd $ filter isCircular
             $ summarize $ takeWhile (<10^6) primes

minimize = read.minimum.circulations.show
circulations = (filter (('0'/=).head)).circ
isCircular (n,c) = (c==)$length$nub$circ$show n
circ s = let len = length s in take len $ circ' len $cycle s
circ' len s@(_:xs) = take len s: circ' len xs
summarize x = let ns = sort$map minimize x in f (2,0) ns where
  f p@(n1,c1) (n2:ns) 
    | null ns   = if n1==n2 then [(n1,c1+1)]    else [p, (n2,1)]
    | otherwise = if n1==n2 then f (n1,c1+1) ns else p:f (n2,1) ns

--55
          
