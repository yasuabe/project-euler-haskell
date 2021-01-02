import Data.List

sumOfAS n= div (n*(n+1)) 2
sumOfAS2 w h=(sumOfAS w)*(sumOfAS h) --(*w')$sum$map (\h'->h') [1..h] where w'=sumOfAS w
f :: Int->Int
f x = ceiling (-(53-1)*(x'-2000)/(2000-52)+1) where x'=fromIntegral x

g x y diff
    | diff' < 0 = if diff < diff' then (x, y+1, diff) else (x, y, -diff')
    | otherwise = g x (y-1) diff'
    where diff' = (sumOfAS2 x y)-2000000

e85 = (\(x,y,_)->x*y)
    $ minimumBy(\(_,_,z1) (_,_,z2)->compare z1 z2)
    $ map (\x->g x (f x) 0) [53..2000]
