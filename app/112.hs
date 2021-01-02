-- TODO: need a bit more refactoring
import Data.List
isBouncy n = original /= sorted && reversed /= sorted
    where original = show n
          reversed = reverse original
          sorted   = sort original

f :: Int->Int->(Double, Int, Int)
f n b | r >= 99   = (r, n, b)
      | otherwise = f (n+1) b'
      where b' = if isBouncy n then b + 1 else b
            r  = (fromIntegral b') *100 / (fromIntegral n)
main = print $ (\(_,ans,_)->ans) $ f 1 0
