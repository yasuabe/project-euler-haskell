import Data.Numbers.Primes
import Data.List
import Data.Maybe

-- a brute force. but complete immediately with optimization.

nthTn n = div ((n+1)*n) 2

f n = f' primes n 1
f' (p:_)  1 r = r 
f' (p:ps) n r = f' ps n' (r*c) where (n', c) = h n p 1
h n p c | 0==mod n p = h n' p (c+1)
        | otherwise  = (n, c)
        where n' = div n p

main = print
     $ fst $ fromJust
     $ find ((500<).snd) $ map (\tn->(tn, f tn)) $ map nthTn [11168..]
--
-- $ ghc 12.hs -O -rtsopts=all
--[1 of 1] Compiling Main             ( 12.hs, 12.o )
--Linking 12 ...
-- $ ./12 +RTS -K500M -RTS
--Just (76576500,576)

-- *** 11168
-- 500 = 2*2*5*5*5
-- 2^4*3^4*5^4*7*11=62370000
-- (sqrt (8*62370000+1)-1)/2 = 11168.206292583756
--

