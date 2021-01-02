import Data.List
import Data.Function

main = print $ fst
     $ minimumBy(\a b->(on compare fst)a b)
     $ concat$filter (\l->5==length l)
     $ groupBy (\a b->(on (==) snd)a b)
     $ sortBy (\a b->(on compare snd) a b)
     $ map ((\s->(s,sort$show$s)).(^3)) [1..10000]
