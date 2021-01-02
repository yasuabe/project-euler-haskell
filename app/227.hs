len = 100
l0 = (replicate 50 0)++[1]++(replicate 49 0)
l1 = map (/36)$[1, 8, 18, 8, 1]++(replicate (len-5) 0)
f1 i = take len$drop (len-i+2) $cycle l1 
f2 i v ls = zipWith (+) ls $ map (v*) $f1 i
f3 ls1 = foldr (\i ls->f2 i (ls1!!i) ls) (replicate 100 0) [0..99]
f4 i lmt e ls1
    | sum ls1 < 1.0e-12 = (e,ls1)
    | otherwise = f4 (i+1) lmt (e+i*h) (0:t)
    where (h:t) = f3 ls1
main = print $ fst $f4 1 100000 0 l0
