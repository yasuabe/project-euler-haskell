import Data.List
import Data.Maybe
import Data.Function

f n = fromJust$find (\m->g (m*n))[1..9999999999999999999999]
  where g l = all (\ch->'0'==ch||'1'==ch||'2'==ch) $ show l

f1 p ns = ms ++ (f1 (succ p) (ns++ms))
  where ms = [lm*10^p+n| lm<-[1,2],n<-ns]  

main = mapM_ print $ sortBy (\a b->on compare snd a b)$ map (\n->(n,f n)) [1..500]
