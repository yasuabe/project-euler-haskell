-- 95 min. need optimization.
import Data.Numbers.Primes
import Data.List

f n l [] = 0
f n l@(hl:tl) r@(hr:tr)
    | s<n       = f n (l++[hr]) tr
    | s>n       = f n tl r
    | otherwise = length l
    where s=sum l
f1 n = f n [h] t where (h:t)=takeWhile (<n) primes

main= print$ maximumBy (\a b->compare (snd a) (snd b))
           $ map (\p->(p,f1 p))
           $ takeWhile (<1000000)$drop 2$primes

-- $ ghc -rtsopts 050.hs; time ./050 +RTS -K2500M -RTS
-- [1 of 1] Compiling Main             ( 050.hs, 050.o )
-- Linking 050 ...
-- (997651,543)
-- 
-- real	95m50.345s
-- user	67m27.266s
-- sys	21m41.787s
