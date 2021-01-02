import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Base(unsafeRead, unsafeWrite, unsafeAt)
import Data.Numbers.Primes

pow n = (toInteger n)^2

muTable :: Int -> UArray Int Int
muTable n = runSTUArray $ do
    arr <- newArray (0,n) (-2)
    mapM_ (g arr) [1..n]
    return arr
  where
    g arr i = do
      v <- unsafeRead arr i 
      when (0/=v) $ if isPrime i
         then mapM_ (\j-> unsafeWrite arr (i^2*j) 0) range >>
              unsafeWrite arr i (-1)
         else let v = if (even.length.primeFactors) i then 1 else -1 in
              unsafeWrite arr i v
      where range = [1..fromInteger$div (toInteger n) (pow i)]

cloitre n = map (\k->(mu k) * (div n (pow k))) [1..rt]
         where rt   = floor$sqrt$fromIntegral n
               mu k = toInteger $ unsafeAt (muTable rt) k

main = print $ sum $ cloitre (2^50)

-- $ ./193 +RTS -K2500M -RTS

