import Data.Ratio
import Data.List

f :: Integer -> Maybe (Ratio Integer)
f n = find ((==n).denominator)
    $ map((%n).(start-)) [0..]
    where start = div (3*n) 7 

e71 :: [Integer] -> Maybe (Ratio Integer)
e71 []     = Just (0%1)
e71 (n:ns) = max (f n) (e71 ns)

main = print$e71 $reverse [7..1000000]
