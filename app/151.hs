-- TODO: refactoring
import Data.List
import Data.Ratio
import qualified Data.Map as M

dec l = dec' (sum l) [] l
dec' d _ []     = []
dec' d h (0:xs)  = (dec' d (h++[0]) xs) 
dec' d h (x:xs) = (h++(x-1: map (+1) xs),x%d): (dec' d (h++[x]) xs)

mdec ls = nub $concatMap ((map fst).dec) ls
g m n = sum $map (\k->(m M.!k) * (xs M.! k)+op) (M.keys xs)
      where xs = M.fromList$ dec n 
            op = if sum n == 1 then 1 else 0

f :: [[Integer]]->M.Map [Integer] (Ratio Integer)
f [l@[0,0,1]] = M.fromList [(l,0%1)]
f [l@[0,0,0,1]] = M.fromList [(l,0%1)]
f ls = M.fromList $map (\l->(l,g x l)) ls where x = f $mdec ls

e151 = snd$head$M.toList$f [[1,1,1,1]]
main = print $ (\r->((fromIntegral.numerator) r)/((fromIntegral.denominator) r))$e151

