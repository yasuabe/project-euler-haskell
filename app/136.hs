import Data.Array.ST
import Control.Monad
import Data.Array.Base

max_n = 50000000

f2 x = takeWhile (\k-> k <= div max_n x && k < 3*x) [min_k, min_k+4..]
    where min_k = 4 - mod x 4

f3 t1 x = forM_ (f2 x) (\k -> f4 k)
  where f4 k = let n = k*x in unsafeRead t1 n >>= \c -> unsafeWrite t1 n (c+1)

pe136 :: UArray Int Int
pe136 = runSTUArray $ do
    t1 <- newArray (0, max_n) 0 
    forM_ [1.. max_n] (f3 t1)
    return t1

main = print $ length $ filter (==1) $ elems $ pe136
