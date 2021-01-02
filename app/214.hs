import Data.Numbers.Primes
import Data.List
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Base(unsafeRead, unsafeWrite, unsafeAt)

phi n  = div (n*(product$map (\n->n-1) ps)) (product ps) where
    ps = map head$group$primeFactors n

e214:: [Integer] -> UArray Int Int
e214 ls = runSTUArray $ do
    a <- newArray (0,40000000) 0
    unsafeWrite a 1 1
    mapM_ (g a) ls
    return a
  where 
    g a n = do
       t <- unsafeRead a $fromInteger n
       if 0<t  then return t
       else do p <- g a (phi n)
               unsafeWrite a (fromInteger n) (p+1)
               return (p+1)
                    
main = print
     $ sum $ map fst $ filter (\(n,t)->isPrime n && 25==t)
     $ zip [0..] $elems 
     $ e214 $ takeWhile (<40000000) primes
-- $ ./214  +RTS -K1500M -RTS
