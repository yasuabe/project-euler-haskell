import Data.Char
pe030 = sum $ filter (\n->n==f' n) [2..350000]
    where f' n = sum $map ((^5).digitToInt) $show n
main = print $ pe030
--443839
