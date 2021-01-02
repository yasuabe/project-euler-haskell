import Data.List
import Data.Numbers.Primes
import Data.Maybe

main = print e51
e51  = div10*10+mod10 where Just (div10, mod10, _) = find'$filter' primes

find'  cands = find (\(d10,m10,c)->8==countMatched d10 m10 c) cands
countMatched d10 m10 ch = length$filter (isPrime) $candsIn (show d10) m10 ch
candsIn s m10 ch = map (\r->((+m10).(*10))$read$replace s ch r) [ch..'9']
replace [] a b  = []
replace (s:ss) a b = (if s == a then b else s):replace ss a b

filter' prms = map (\(p,r)->(div p 10, mod p 10, fromJust r))
             $ filter (isJust.snd) 
             $ map (\p->(p,scan3 (div p 10))) prms
scan3 s = scan3' 0 0 0 (show s) where
    scan3' 3 _ _ []    = Just '0'
    scan3' _ 3 _ []    = Just '1'
    scan3' _ _ 3 []    = Just '2'
    scan3' _ _ _ []    = Nothing
    scan3' c0 c1 c2 ('0':ns) = scan3' (c0+1) c1 c2 ns 
    scan3' c0 c1 c2 ('1':ns) = scan3' c0 (c1+1) c2 ns 
    scan3' c0 c1 c2 ('2':ns) = scan3' c0 c1 (c2+1) ns 
    scan3' c0 c1 c2 (_:ns)   = scan3' c0 c1 c2     ns

-- $ ./51 +RTS -K500M -RTS
-- 121313
--
