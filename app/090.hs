-- need refactor
import Data.List

sqrs ::[(Int,Int)]
sqrs = [(0,1),(0,4),(0,6),(1,6),(2,5),(3,6),(4,6),(6,4),(8,1)]
f [] = [([],[]),([],[])]
f ((l,r):t) = do 
    (ls,rs) <- f t
    let  s1=((l:ls),(r:rs)); s2=((r:ls),(l:rs))
    [s1,s2]
g s@(e1:e2:e3:e4:e5:e6:[]) = [sort s]
g s=([0..9]\\s)>>=(\e->g (e:s))       
h (s1,s2)=[(s1',s2')|s1'<-g s1, s2'<-g s2]

list2= nubBy (\(s11,s12) (s21,s22)->(s11==s22) && (s12==s21))$f sqrs
list3 = map (\(l1, l2)->(sort$nub l1, sort$nub l2)) list2
list4 = nubBy (\(s11,s12) (s21,s22)->s11==s21&&s12==s22||s11==s22&&s12==s21)$list3
list5 = filter (\(s1,s2)->length s1 <=6 && length s2<=6)list4
list6=concatMap (\(s1,s2)->if elem 6 s2 then [(s1,s2),(s1,(++[9])$delete 6 s2)] else [(s1,s2)]) list5
list7=concatMap (\(s1,s2)->if elem 6 s1 then [(s1,s2),((++[9])$delete 6 s1,s2)] else [(s1,s2)]) list6

main =print$length$nubBy (\(s11,s12) (s21,s22)->s11==s21&&s12==s22||s11==s22&&s12==s21)$concatMap h list7
