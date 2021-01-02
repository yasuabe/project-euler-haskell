e101' n = 1 - n + (n^2) - (n^3) + (n^4) - (n^5) + (n^6) - (n^7) + (n^8) - (n^9) + (n^10)

e101 m@(r:rs) = (r: map (0:) (e101 m'))
    where m' = map (reduce' r) rs
          reduce' (r:rs) (r':r's) = zipWith (\e e'->e'*r-e*r') rs r's

f ((an:bn:[]):[]) = [div bn an]
f (row@(an:as):rows)  = ((div ((last row)-(sum (zipWith (*) as sols))) an):sols) where sols = f (map tail rows)

g :: [Integer]->Integer->Integer
g bs = (\n->let pm = ((length bs) -1) in sum $zipWith (\a b->a*n^b) bs [pm, pm-1..])

h :: [Integer] -> Integer
h bs = g bs ((fromIntegral (length bs))+1)

m n = [[r^(n-c)|c<-[1..n]] |r<-[1..n]]

f1 n = zipWith (\a b-> a++[b]) (m n) (map e101' [1..])

--ghci> sum$map (\n->h (f$e101 $f1 n)) [1..10]
--37076114526

