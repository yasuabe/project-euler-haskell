--TODO: too slow. need performance tuning (spent about 4 days)
--TODO: need better algorithm
import qualified Data.Set as S
import Data.List
--f :: [S.Set a]->[(Int,[a])]->[Maybe [S.Set a]]
f ss []         = [ss]
f ss ((n,ls):t) = if null cs
                  then []
                  else filter (\m->all ((<10).S.size) m)$ concatMap (\c->f (g2 c ss ls) t) cs
    where idxs = g ss ls
          cs   = f2 idxs n
s0 = S.fromList ['0'..'9']     
ss = replicate 16 S.empty
g ss ls
    = filter (/=(-1))
    $ zipWith3 (\i s e->if S.notMember e s then i else -1) [0..] ss ls
f2 idxs 0 = [[]]
--f2 idxs 1 = map (:[]) idxs
f2 idxs n = concatMap (\(h:t)->map (h:) $f2 t (n-1))
         $ filter ((n<=).length)
         $ tails idxs
g2 cs ss ls = zipWith3 (\i s e->
    S.union (if elem i cs then S.delete e s0 else S.singleton e) s
    ) [0..] ss ls
q2 :: [(Int,String)]
q2 =[(2,"90342"),
     (0,"70794"),
     (2,"39458"),
     (1,"34109"),
     (2,"51545"),
     (1,"12531")]

q :: [(Int,String)]
q = [(2,"5616185650518293"),
     (1,"3847439647293047"),
     (3,"5855462940810587"),
     (3,"9742855507068353"),
     (3,"4296849643607543"),
     (1,"3174248439465858"),
     (2,"4513559094146117"),
     (3,"7890971548908067"),
     (1,"8157356344118483"),
     (2,"2615250744386899"),
     (3,"8690095851526254"),
     (1,"6375711915077050"),
     (1,"6913859173121360"),
     (2,"6442889055042768"),
     (0,"2321386104303845"),
     (2,"2326509471271448"),
     (2,"5251583379644322"),
     (3,"1748270476758276"),
     (1,"4895722652190306"),
     (3,"3041631117224635"),
     (3,"1841236454324589"),
     (2,"2659862637316867")]

main = mapM_ print$f ss q
--ghci>let s = [fromList "012356789",fromList "012345789",fromList "012356789",fromList "123456789",fromList "013456789",fromList "012345789",fromList "023456789",fromList "012346789",fromList "012345689",fromList "023456789",fromList "012345679",fromList "012356789",fromList "012345678",fromList "012346789",fromList "012456789",fromList "012456789"]
--ghci> Data.List.concatMap (((Data.List.\\)['0'..'9']).elems) s
--"4640261571849533"
