import Control.Monad
import Control.Monad.ST
import Data.Array.ST

maxlen = 50

mkArray :: (ST s) (STArray s (Int,Int) Integer)
mkArray = newArray ((0,0),(maxlen,maxlen)) 0

cmb :: [(Int,Int)]
cmb = [(a,b)|a<-[1..div maxlen 4], b<-[a-1..maxlen-a*3]]

e114 ls = runST $ do
    a1 <- mkArray
    a2 <- mkArray
    mapM (\(r,b)->g a1 a2 r b) ls
  where
    g a1 a2 r b = do c1<-f1 a1 r b
                     c2<-f2 a2 (maxlen-3*r-b) r
                     return $c1*c2
    f1 a 0 _ = return 1
    f1 a r b | r == b+1  = return 1
             | otherwise = do 
                  c <- readArray a (r,b)
                  if 0 < c then return c
                  else do c1 <- (f1 a (r-1) (b-1))
                          c2 <- (f1 a r (b-1))
                          let c3 = c1 + c2
                          writeArray a (r,b) c3
                          return c3
    f2 a 0 _   = return 1
    f2 _ _ 0   = return 0
    f2 a r1 r2 = mapM (\s->f2 a (r1-s) (r2-1)) [0..r1]>>=(return.sum)

main =print $(+1)$sum$e114 cmb
