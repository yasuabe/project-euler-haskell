import Control.Monad
import Control.Monad.ST
import Data.Array.ST

e116 = runST $ do 
    a <- newArray ((0,0),(50,50)) 0 ::(ST s) (STArray s (Int,Int) Integer)
    mapM (g a) [2,3,4]>>=(return.sum)
  where
    g a w   = mapM (\p->f a (50-w*p) (p+1)) [1..div 50 w] >>=(return.sum)
    f a n 1 = return 1
    f a n p = do 
        c <- readArray a (n,p)
        if 0 < c then return c
        else do b <-mapM (\m->f a (n-m) (p-1))[0..n]
                let s = sum b
                writeArray a (n,p) s
                return s
--ghci> e116
