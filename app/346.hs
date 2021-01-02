import qualified Data.Set as S
import Data.List

f lim n c s
  | lim < c   = s
  | otherwise = f lim n (n*c+1) (S.insert c s)
g lim n s = f lim n (n^2+n+1) s
h lim = S.toList $ foldl' (\s n->g lim n s) S.empty [2..ub]
  where ub = floor $ (-1+sqrt(1+4*((fromIntegral lim)-1)))/2

main = print $ (1+) $ sum $ h (10^12)
