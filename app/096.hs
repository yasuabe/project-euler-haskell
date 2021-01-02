-- half machine, half manual (try-and-error)
-- need refactoring
import Data.List

group1 = [(0,6,'9'),(0,8,'7'),(1,3,'4'),(1,4,'2'),(1,6,'1'),(1,7,'8'),(2,3,'7'),(2,5,'5'),(2,7,'2'),(2,8,'6'),(3,0,'1'),(3,3,'9'),(3,5,'4'),(4,1,'5'),(4,7,'4'),(5,3,'5'),(5,5,'7'),(5,8,'9'),(6,0,'9'),(6,1,'2'),(6,3,'1'),(6,5,'8'),(7,1,'3'),(7,2,'4'),(7,4,'5'),(7,5,'9'),(8,0,'5'),(8,2,'7')]::[(Int,Int,Char)]
ini = take 9$repeat$take 9$repeat ['1'..'9']
split idx l = let (l1,(h:t)) = splitAt idx l in (l1, h, t)

setAt idx e l = l1 ++ (e:l2) where (l1, _, l2) = split idx l

setAt' r c e mtx = setAt r (setAt c e (mtx!!r)) mtx 

removeFromRow r n mtx = setAt r newRow mtx
    where newRow = map (delete n) $mtx!!r

removeFromCol c n mtx = map (\row->
    let new = delete n (row!!c) 
    in setAt c new row) mtx

removeFromArea r c n mtx = f (r'+2) r' c' n mtx
    where r' = (div r 3) * 3
          c' = (div c 3) * 3
f rlmt rx c n mtx | rlmt < rx = mtx
                  | otherwise = let clmt=c+2; mtx' = g clmt rx c n mtx in f rlmt (rx+1) c n mtx'
g clmt rx cx n mtx | clmt < cx = mtx
                   | otherwise = let old = mtx!!rx!!cx;mtx' = setAt' rx cx (delete n old) mtx in g clmt rx (cx+1) n mtx'
setAt'' r c n mtx= setAt' r c [n] mtx''' where
    mtx'   = removeFromRow  r n mtx
    mtx''  = removeFromCol  c n mtx'
    mtx''' = removeFromArea r c n mtx''

apply []            mtx = mtx
apply ((r, c, d):t) mtx = let mtx' = setAt'' r c d mtx in apply t mtx'

f1 _ []                           = []
f1 s1@(ch:[]) (s2:t2) | s1 == s2  = s2: remainder
                      | otherwise = (delete ch s2): remainder
    where remainder = f1 s1 t2

f2 []            ss = ss
f2 (s@(ch:[]):t) ss = f2 t $f1 s ss
f2 (_:t)         ss = f2 t ss

f3 l | l == l'   = l'
     | otherwise = f3 l'
    where l' = f2 l l
f4 mtx = map f3 mtx

f5 mtx = transpose $ f4 $ transpose mtx
f9 f grid = transpose $ (f8 f7) $ transpose grid
f6 grid = foldr (\(r,c) g->setArea r c (f3$area r c g) g) grid [(r,c)|r<-[0..2],c<-[0..2]]
f10 f grid = foldr (\(r,c) g->setArea r c (f$area r c g) g) grid [(r,c)|r<-[0..2],c<-[0..2]]

f7 l = map (\s->let c=intersect b s in if 1<length s&&(not.null) c then c else s) l
    where b = map fst$filter ((==1).snd)$map (\d-> (d,foldr (\s a->if elem d s then a + 1 else a) 0 l))['1'..'9']

f8 f grid = map f grid


g1 mtx | mtx == mtx' = mtx'
       | otherwise   = g1 mtx'
    where mtx' = ((f10 f7).(f9 f7).(f8 f7).f6.f5.f4) mtx

row i grid = grid!!i
col i grid = (transpose grid)!!i
area' i grid = area v h grid where v=div i 3; h=mod i 3
check cells=(length cells')==(length$nub cells')where cells'=filter ((1==).length) cells
checkAll grid = all check3 [0..8]
    where check3 i = (check$row i grid) && (check$col i grid) && (check$area' i grid)

area v h grid = concatMap (\r->map (\c->grid!!(v'+r)!!(h'+c))[0..2]) [0..2] 
    where v'=v*3; h'=h*3
setArea v h cells grid = upper ++ new ++ lower 
    where (upper, old, lower) = split3 v grid
          new = zipWith (\r row->let (c1,_,c3)=split3 h row in c1++r++c3) cells' old 
          cells' = [take 3 cells, drop 3$take 6 cells, drop 6 cells]

split3 i l = (take i' l, drop i'$take i'' l, drop i'' l) where i'=i*3; i''=(i+1)*3

load = do 
    cnt <- readFile "sudoku.txt"
    let qs = lines cnt
    return$map processLines$groupBy10 qs

groupBy10 [] = []
groupBy10 lns = take 10 lns:(groupBy10 $drop 10 lns)

processLines lns = 
    let lns' = drop 1 lns in
    concatMap (\lx->processLine lx (lns'!!lx))[0..8]
processLine lx line
    = filter (\(_, _, c)->'0'/=c)
    $ map (\cx->(lx, cx, line!!cx)) [0..8]

--1	483
--2	245
--3	462
--4	137
--5	523
--6	176
--7	143
--8	487
--9	814
--10	761
--11	976
--12	962
--13	397
--14	639
--15	697
--16	361
--17	359
--18	786
--19	743
--20	782
--21	428
--22	425
--23	348
--24	124
--25	361
--26	581
--27	387
--28	345
--29	235
--30	298
--31	761
--32	132
--33	698
--34	852
--35	453
--36	516
--37	945
--38	365
--39	134
--40	193
--41	814
--42	384
--43	469
--44	316
--45	586
--46	954
--47	159
--48	861
--49	294
--50	351
--	24702
-- mapM_ (\n->print (n+1)>>load>>=(\l-> mapM_ print$g1 $apply (l!!n) ini)) [0..49]
