-- TODO:  要リファクタ
-- STモナドを使わなくても書ける

import Data.STRef
import Control.Monad.State
import qualified Data.Map as M
import Control.Monad.ST
import Text.Printf (printf)

solve :: Int -> Double
solve m = runST $ newSTRef M.empty >>= (e m) where
  e n ref
    | n == 0    = return 0
    | otherwise = do k <- zipWithM (\a b-> fmap (a*) (f b)) as [0..]
                     return $ ((2^n) + (sum k)) / (2^n-1)
    where
      as = (reverse.tail.binCoeffs) n
      binCoeffs n
        | n == 0    = [1]
        | otherwise = zipWith (+) l (reverse l) 
        where l     = 0:binCoeffs (n-1)
      f n = do m <- readSTRef ref
               case M.lookup n m of
                   Just a  -> return a
                   Nothing -> do a <- e n ref 
                                 writeSTRef ref (M.insert n a m)
                                 return a
main = printf "%.10f\n" $ solve 32
