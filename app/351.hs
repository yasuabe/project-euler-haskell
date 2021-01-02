{-# LANGUAGE FlexibleContexts #-}
import Data.Array.Base(unsafeRead, unsafeWrite, unsafeAt)
import Data.Numbers.Primes
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Debug.Trace

limit   = 100000000::Int
exceeds = ((toInteger limit)<)

phiArray :: UArray Int Int
phiArray = runSTUArray
         $ newArray (0, limit) 1 >>= (flip f2 2) where
  f2 arr i
    | exceeds i = return arr
    | otherwise = do
        n <- readAt i 
        when (1 == n) $ f3 i i >> f4 i (i^2)
        f2 arr (i+1)
    where
      f3 i j
        | exceeds j = return ()
        | otherwise = modifyAt j (*(i-1)) >> f3 i (j+i)
      f4 i p
        | exceeds p = return ()
        | otherwise = f5 i p p >> f4 i (p*i)
      f5 i p j
        | exceeds j = return ()
        | otherwise = modifyAt j (*i) >> f5 i p (j+p)
      readAt i      = unsafeRead arr (fromInteger i) >>= (return.toInteger)
      modifyAt i f  = do
        n <- readAt i
        unsafeWrite arr (fromInteger i) ((fromInteger.f) n)

solve arr n acc
  | limit < n = acc
  | otherwise = solve arr (n+1) (acc+toInteger(n-phi))
  where phi   = unsafeAt arr n

main = print $ 6 * (solve phiArray 1 0)
