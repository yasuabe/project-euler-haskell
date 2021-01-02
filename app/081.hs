import qualified Data.Text as T

tilt ct  = map (map (\(r,c)->ct!!r!!c))$f $length (ct!!0) where
    f w = map (\dst->[(r,dst-r)|r<-[0..dst], r<w, dst-r<w]) [0..(2*(w-1))]

sumMin (x1:x2:[]) = [addMin x1 x2]
sumMin (x1:x2:xs) = (sumMin ((addMin x1 x2):xs))

addMin x1@(h1:t1) x2@(h2:t2)
    | length x1 == 1        = zipWith (+) x2 $repeat h1
    | length x1 < length x2 = zipWith (+) x2 $(h1:(zipWith min x1 t1)++[last t1])
    | otherwise             = zipWith (+) x2 $zipWith min x1 t1

main = do
    contents <- readFile "files/matrix.txt"
    print $ sumMin $ tilt $parseMatrix contents
  where
    parseMatrix  = (map (\r->map(read.T.unpack) r)).parseRow 
    parseRow     = (map ((T.split (==',')).T.pack)).lines 
