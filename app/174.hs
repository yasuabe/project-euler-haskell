import Data.List
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Base(unsafeRead, unsafeWrite, unsafeAt)

g :: UArray Int Int
g = runSTUArray$ do arr <- newArray (0, 250000) 0
                    mapM_ (f1 arr) [1..499]
                    return arr
  where f1 arr d = f arr d (a0 d)
        a0 n     = div ((2*1+(n-1)*2)*n) 2 

f arr d a 
    | 250000<idx  = return ()
    | otherwise   = do cur <- unsafeRead arr idx
                       unsafeWrite arr idx (cur+1)
                       f arr d idx
    where idx = a+d

main = print$sum$take 10$drop 1
     $ map (\l->length l)$group$sort$elems g
