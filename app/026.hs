import Data.List
pe026 n d rs
    | r == 0    = []
    | elem r rs = dropWhile (/=r)$ reverse rs
    | otherwise = pe026 (r * 10) d (r:rs)
    where r = mod n d
main = print
     $ fst
     $ maximumBy (\(_,l1) (_,l2)->compare l1 l2)
     $ map (\d->(d,length$pe026 1 d [])) [1,3..1000]
--ghci> main
--983

