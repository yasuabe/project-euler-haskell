-- brute force
import Data.List
import Data.Maybe
import Data.Ratio
pe033 = [f(n,d)|n<-[10..99],d<-[10..99]] where
    f (n,d) | n>=d||n1==0||d1==0  = Nothing
            | n1==d2&&n*d1==d*n2 = Just (n,d)
            | otherwise          = Nothing
        where n1=mod n 10;n2=div n 10;d1=mod d 10;d2=div d 10
main = print $ denominator
     $ foldr1 (*)
     $ map ((\(n,d) -> n%d).fromJust)
     $ filter (not.isNothing) pe033          
