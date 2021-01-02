-- TODO: too slow. need optimization
import Data.List
import Data.Maybe

readBin n [] = n
readBin n ('0':s) = readBin (2*n) s
readBin n ('1':s) = readBin (2*n+1) s

g cs     []        = Just cs
g (c:cs) (b:bs)
  | c==b || c=='_' = case g cs bs of
                       Nothing -> Nothing
                       Just s  -> Just (b:s)
  | otherwise      = Nothing

f l cs bs d
  | l == d    = []
  | otherwise = case cur of
                  Nothing -> next
                  Just s  -> let (h,t)=splitAt (l-d) s in (t++h):next
  where
    cur  = let (h,t) = splitAt d cs in g (t++h) bs
    next = f l cs bs (d+1)     

h l ([],    ss) = ([],ss)
h l ((b:bs),ss) = h l (bs, concatMap (\s->f l s b 0) ss)

h1 bs = [bh:bt|bt <- bs, bh <- "01"]
ss = tail $ foldr (\f a->f a) [[]] $ replicate 4 h1
h2 n = (n, "0000" ++ (replicate (n-4) '_'))

pe265 = snd $ fromJust $ find (\(_,b)->(not.null) b) $ map (\(n,s)->h n (ss, [s])) $ map h2 [5..]
main = print $ sum $ map (readBin 0) pe265 

-- $ time ./265  +RTS -K2500M -RTS
-- ???
--
--real	43m42.746s
--user	43m19.342s
--sys	0m3.323s
