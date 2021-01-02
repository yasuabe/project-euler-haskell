import Data.Array.ST
import Data.Array.IArray
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.ST
import Data.Array.Base
import qualified Data.Map as Map 

max_n = 1000000

f1 n d q = if 3 * d^2 < n && 3 * d - q /= -d + q then 2 else 1

f2 d = takeWhile (\(n, _)->n < max_n)
     $ map (\q -> (q * (4 * d -q), q)) [1..2*d]

f3 t d = forM_ (f2 d) (\(n, q)-> f4 n q)
  where
    f4 n q = unsafeRead t n >>= \a -> unsafeWrite t n (a + (f1 n d q))

pe135 :: UArray Int Int
pe135 = runSTUArray $ do
    t <- newArray (1, max_n) 0
    forM_ [1..div max_n 4] $ \d -> f3 t d
    return t

main = do
    print $ length $ filter (==10) $ elems $ pe135
