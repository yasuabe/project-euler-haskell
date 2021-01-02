import Data.List

f n = filter judge [10^(n-1)..(div (10^n) 6)]
    where judge n = g 1==g 2&&g 1==g 3&&g 1==g 4&&g 1==g 5&&g 1==g 6
                  where g m=sort$show$n*m

main = do let Just (ans:_) = find (not.null) $ map f [1..]
          print ans

