e67 acc (rh:[])      = add acc rh
e67 acc rows@(rh:rt) = e67 (add acc rh) rt 
add acc row = e67' ((0:acc) ++ [0]) row where
    e67' _ [] = []
    e67' (m1:m2:ms) (eh:et) = (eh + max m1 m2): e67' (m2:ms) et
main = do cnt <- readFile "files/p067_triangle.txt"
          print $ maximum $ e67 [] $ triangle $ lines cnt
    where
       triangle ls = map (\line->map (read::String->Integer) $ words line) ls
