-- TODO: too slow (7 min)
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Base(unsafeRead, unsafeWrite, unsafeAt)
import System.Environment

mark :: Int -> Int -> UArray Int Bool
mark n m = runSTUArray $ do
  arr <- newArray (0, n) False
  loop arr 1 3
  where
    loop arr a da
      | a > n = return arr
      | otherwise = do
          loop2 arr m (3*m) 
          loop arr (a+da) (da+2)
      where
        loop2 arr b db
          | lmt < b   = return ()
          | otherwise = do unsafeWrite arr (a+b) True
                           loop2 arr (b+db) (db+2*m)
          where lmt = n - a

markAll n = [mark n 1,mark n 2,mark n 3,mark n 7]

count :: Int -> [UArray Int Bool] -> Int
count n as@(a1:a2:a3:a4:[]) = loop 2 0
  where
    loop k acc
      | k > n = acc
      | all (\a->unsafeAt a k) as = loop (k + 1) (acc + 1)
      | otherwise = loop (k + 1) acc

main = do
  [n] <- fmap (map read) getArgs
  print $ count n $ markAll n
