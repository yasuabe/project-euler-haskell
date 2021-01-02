import Data.Numbers.Primes
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Debug.Trace

type Digits = [Int]

toNumber::Digits->Int
toNumber = foldr1 (\a b -> b*10 + a)

perm 0 _      _  = [[]]
perm _ []     _  = []
perm n (c:cs) ns = (map (c:) $ perm (n-1) (cs++ns) []) ++ (perm n cs (c:ns))

combinations :: Int->(Digits,Digits)->[Digits]
combinations n ([],      d2) = []
combinations n ((d1:ds1),d2) = (map (d1:) $perm (n-1) (ds1++d2) []) ++ combinations n (ds1,(d1:d2)) -- perm n cand []

f1::Int->Digits->[(Int,Digits)]
f1 1 ds = onlyPrime (\x ->(x, delete x ds)) $ intersect ds [2,3,5,7]
f1 n ds = case n==length ds && mod (sum ds) 3 == 0 of
  True  -> []
  False -> onlyPrime (\x ->(toNumber x, ds\\x)) $ combinations n (its, exc)
  where
    its = ds `intersect` [1,3,7,9]
    exc = ds \\          [1,3,7,9]

onlyPrime f l = filter (isPrime.fst) $ map f l

type Memo = M.Map (Int,Digits) [[Int]]

f2:: Int->Int->Digits->State Memo [[Int]]
f2 root 0 remnant = return [[root]]
f2 root n remnant = fmap (map (root:)) $ f3 n remnant
  
--f3 3 [1,3,4] = [[431],[41,3]]
f3::Int->Digits->State Memo [[Int]]
f3 0 remnant = return []
f3 n remnant = do
   memo <- get
   case M.lookup key memo of
    Just x  -> return   x
    Nothing -> do 
                next    <- f3 (n-1) remnant
                current <- mapM callF2 $ f1 n remnant
                let x'  = (concat current)++next
                modify (M.insert key x')
                return x'
  where
    key = (n, sort remnant)
    callF2 (root,rem) = f2 root (min (length rem) n) rem

main::IO ()
main = do print $ length $ nub $ map sort
                $ evalState (f3 9 [1,2,3,4,5,6,7,8,9]) M.empty
