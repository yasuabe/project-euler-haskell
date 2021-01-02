import Data.List
f1 = maximum $ map f2 [2..9]
f2 n = (\ns->if not $null ns then maximum ns else [])$
              filter isPandigital$
              takeWhile ((<=9).length) $
              dropWhile ((<9).length)  $
              map (\x->(concatMap (show.(*x)) [1..n])) [1..]
    where
        isPandigital s = "123456789" == sort s
main = print $ f1
--"932718654"

