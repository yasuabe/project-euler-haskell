-- http://www.wizforest.com/gear/tiger/sqrt/
-- minuend subtrahend count
f m s c | m < s     = (s-2,m,c)
        | otherwise = f (m-s) (s+2) (c+1)
g m s   = c :g (d*100) ((s'+1)*10+1)
        where (s',d,c) = f m s 0

e80 = sum$map (\n->sum$take 100$g n 1)$filter isNotSquare [1..100]
    where isNotSquare n = (round.sqrt$fromIntegral n)^2/=n
main =print $ e80
