import Data.Maybe
import Data.List
f 'D' a = Just (3*a)
f 'U' a = if 2==mod a 4 then Just (div (3*a-2) 4) else Nothing
f 'd' a = if 1==mod a 2 then Just (div (3*a+1) 2) else Nothing
apply [] n = Just n
apply (s:ss) n
  = case next of
      Just n -> apply ss n
      Nothing -> Nothing
  where next = f s n
main = print $ find (\m->10^15 < fromJust m)
     $ filter isJust$map (apply "dDUUdDDUDDddDdDddDDUDDdddUDDDU") [1..]
