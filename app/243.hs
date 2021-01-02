-- TODO: refactor
import qualified Data.Map as M
import Data.Ratio
import Data.Numbers.Primes

phi fs = product $map phi' fs  where
    phi' (p, k) = p^k - (p^(k-1))

factors n = factors' n primes M.empty where
    factors' 1 _           m        = m
    factors' n prms@(p:ps) m
        | 0 ==mod n p && M.member p m = let k = (M.!) m p;map' = M.insert p (k+1) m in factors' (div n p) prms map'
        | 0 ==mod n p                 = let map' = M.insert p 1 m in factors' (div n p) prms map'
        | otherwise                 = factors' n ps m
multiply ::M.Map Integer Integer->M.Map Integer Integer->M.Map Integer Integer
multiply  m n = multiply' m (M.toList n) where
  multiply' m []      = m
  multiply' m ((p, k):ns)
    | M.member p m = let k' = m M.! p; m' = M.insert p (k+k') m in multiply' m' ns
    | otherwise    = let m' = M.insert p k      m in multiply' m' ns

f :: [(Integer,Integer)]->Integer->Double
f prms n = (fromIntegral (phi $ M.toList $ multiply (M.fromList prms) (factors n))) /((product'*(fromIntegral n)) - 1)
    where
        product' = fromIntegral$fst$foldr (\(p1,_) (p2,_)->(p1*p2,0)) (1,0) prms 

main = print $(\(n,_)->n * (product $map fst prms))$head $dropWhile (\(n,r)->(15499/94744)<r) $ map (\n->(n,f prms n)) [1..] 
    where prms =  [(2,1),(3,1),(5,1),(7,1),(11,1),(13,1),(17,1),(19,1),(23,1)] 
