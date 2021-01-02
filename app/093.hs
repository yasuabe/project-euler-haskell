--need refactor
import Data.List
import Data.Function

ops = [(+),(-),(*),(/)]

f1 (o1,o2,o3) (v1:v2:v3:v4:[]) = o1 (o2 (o3 v1 v2) v3) v4
f2 (o1,o2,o3) (v1:v2:v3:v4:[]) = o1 (o2 v1 (o3 v2 v3)) v4
f3 (o1,o2,o3) (v1:v2:v3:v4:[]) = o1 (o2 v1 v2) (o3 v3 v4)
f4 (o1,o2,o3) (v1:v2:v3:v4:[]) = o1 v1 (o2 (o3 v2 v3) v4)
f5 (o1,o2,o3) (v1:v2:v3:v4:[]) = o1 v1 (o2 v2 (o3 v3 v4))

a l= [f (o1, o2, o3) perms|o1<-ops, o2<-ops, o3<-ops, f<-[f1,f2,f3,f4,f5],perms<-permutations l]

e93 nums=length$takeWhile (\(i,v)->i==(floor v)) $map (\i->(i,list!!(i-1)))[1..(length list)] where list=sort$nub$filter (\r->1<=r&&(fromIntegral (round r))==r) $a nums

main = print
     $ floor $ foldl (\acc n->10*acc+n) 0 $ fst
     $ maximumBy (on compare snd)
     $ map (\nums->(nums,e93 nums)) [[v1,v2,v3,v4]|v1<-[1..9],
                                                   v2<-[2..9],
                                                   v3<-[3..9],
                                                   v4<-[4..9],
                                                   v1<v2, v2<v3, v3<v4]
