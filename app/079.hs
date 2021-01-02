import System.IO
import Data.List
import Data.Maybe

main = readFile "files/keylog.txt" >>= (print.(!!0).fromJust.e79.lines)

e79 ss = find (not.null) $ map (tryAllKeyLogs ss) [3..]

tryAllKeyLogs (keylog:[]) _          = [keylog]
tryAllKeyLogs  (keylog:keylogs) limit =
    [q|p<-tryAllKeyLogs keylogs limit, q<-combine keylog p limit]

combine candidate keylog lmt = combine' 0 keylog candidate lmt where
  combine' _ []       t lmt = [t]
  combine' a s@(x:xs) t lmt =
    [q | p <- [a..(length t)],
         q <- combine' (p+1) xs (insertAt t x p) lmt, length q<lmt] where
    insertAt s n p =
        let (l,r) = splitAt p s 
        in case r of
            []     -> s ++ [n]
            (x:xs) -> l++(if n == x then r else [n] ++ r)

