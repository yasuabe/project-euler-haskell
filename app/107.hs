-- TODO:refactor
import Data.List
import Data.Maybe
import Data.String.Utils
type Edge = ((Int,Int),Int)

ls :: [Edge]
ls = [((1,2),16),((1,3),12),((1,4),21),((2,4),17),((2,5),20),((3,4),28),((3,6),31),((4,5),18),((4,6),19),((4,7),23),((5,7),11),((6,7),27)]
f1 :: Edge->[Edge]->Maybe Edge
f1 newE@((l,r),w) edges  = f' l r newE edges

f' next goal maxE [] = Nothing
f' next goal maxE edges@(e@((l,r),w):es) =
      if isJust res then fromJust res else Nothing 
      where
        found = filter (connected next) edges 
        res = find isJust $map (\fnd->f'' fnd next goal maxE edges) found
max' e1@(_,w1) e2@(_,w2) = if w1<w2 then e2 else e1
f'' e@((l,r),w) next goal maxE edges =
        let to = (if l==next then r else l) in
        if l==goal||r==goal
          then Just $max' e maxE
          else f' to goal (max' e maxE) (del e edges)
del e edges = deleteBy (\((l1,r1),w) ((l2,r2),_)->l1==l2&&r1==r2||l1==r2&&l2==r1)  e edges
connected next ((l,r),_) = l==next||r==next
f2 newE edges = if isNothing maxE then (newE:edges) else delete (fromJust maxE) (newE:edges) where maxE = f1 newE edges

main = do cnt<-readFile "files/network.txt"
          mapM_ print $foldr f2 [] $f5$collect2 $map toList$lines cnt
toList s = map (\w->if w=="-" then Nothing else Just $(read::String->Int) w) $split "," s

collect []            _ = []
collect ((w:ws):rows) r  = (collect' ws r 1) ++ (collect (map tail rows) (r+1))
    where
          collect' row@(w:ws) r c = ((if isJust w then Just ((r,r+c),fromJust w) else Nothing):collect' ws r (c+1))
          collect' []         _ _ = []
collect2 rows = map fromJust $filter isJust $collect rows 1
has n ((l,r),_)= n==l||n==r
f3 n ls = (\(a,b)->(map (\x->(cp n x,x)) a,b)) $ (filter (has n) ls, filter (not.(has n)) ls)

cp n ((l,r),w)= if l==n then r else l

f4 (l,           [])   = l
f4 ((h@(c,e):t),rest) = let (n,rest') = f3 c rest in (h:f4 ((t++n),rest'))

f5 l = map snd$ f4 (f3 1 l)

raw = do cnt<-readFile "files/network.txt"
         return$length$ foldr f2 []$f5$collect2$map toList$lines cnt

raw' = do cnt<-readFile "files/network.txt"
          print ((original cnt) - (reduced cnt))
       where 
             original cnt = foldr (\((l,r),w) i->i+w) 0$f5$collect2$map toList$lines cnt
             reduced  cnt = foldr (\((l,r),w) i->i+w) 0$ foldr f2 []$f5$collect2$map toList$lines cnt

--ghci> raw'

