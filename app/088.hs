--need better algorithm
--need refactor
import Data.Maybe
import qualified Data.IntSet as S
fk1 a p s 
           | a==1 && p==1 = Nothing
           | 0 == mod sk (pk-1) = let ak=div sk (pk-1) in Just$ak*pk
           | otherwise = Nothing
           where sk = a + s; pk = a * p
callFk1 ak2 p s = map (\m->fk1 m p s)[ak1,(ak1-1)..ak2]
    where ak1 = floor ((1+(sqrt(1+(fromIntegral p) *(fromIntegral s))))/fromIntegral p)

f k n a p s
              | otherwise      = rslt
    where rslt = if n == k-2 then callFk1 a (a*p) (a+s)
                             else f' k (n+1) a (a*p) (a+s)
f' :: Int->Int->Int->Int->Int->[Maybe Int]
f' k n a p s =
    concatMap (\m->f k n m p s) [start,(start-1)..a]
    where start = floor((fromIntegral$div k p)**(1/(fromIntegral (k-n))))
f0 k = f' k (t+1) 1 p s
    where t = (k-1) - floor ((log$fromIntegral k)/(log 2))
          s = t
          p = 1
e87' k= map fromJust$filter isJust$f0 k
main = print$sum$S.toList$S.fromList$4:6:map (minimum.e87') [4..12000]
