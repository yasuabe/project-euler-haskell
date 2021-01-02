import Data.Set as S
import Data.List as L

f1 0 = 0
f1 n = (mod n 10)^2 + (f1 $ div n 10)

f2 1  = 0 
f2 89 = 1
f2 n  = f2 $ f1 n

countF2 :: Int -> Int
countF2 0 = 0
countF2 n = f2 n + (countF2 $ n - 1) 

iniSets  = (S.fromList [1], S.fromList [89])

type Sets = (S.Set Integer, S.Set Integer)

f2' :: Integer->Sets->(Bool,Sets)
f2' n (set1, set89)
    | S.member n set1  = (False, (set1, set89))
    | S.member n set89 = (True,  (set1, set89))
    | otherwise         =
        let (f, (s1, s89)) = f2' (f1 n) (set1, set89)
            s1'  = if f then s1           else S.insert n s1
            s89' = if f then S.insert n s89 else s89
        in (f, (s1', s89'))

countF2' 0 sets = sets
countF2' n sets =
    let (f, sets') = f2' n sets
    in countF2' (n-1) sets'

f2'' :: Integer->Sets->(Bool,Sets)
f2'' n (set1, set89)
    | S.member canon set1  = (False, (set1, set89))
    | S.member canon set89 = (True,  (set1, set89))
    | otherwise         =
        let (f, (s1, s89)) = f2'' (f1 n) (set1, set89)
            s1'  = if f then s1                 else S.insert canon s1
            s89' = if f then S.insert canon s89 else s89
        in (f, (s1', s89'))
       where canon = canonicalize n

canonicalize :: Integer -> Integer
canonicalize n = read $L.filter (\n->n/='0' )$sort $show n

countF2'' 0 sets = 0
countF2'' n sets =
    let (f, sets') = f2'' n sets
    in (if f then 1 else 0) + countF2'' (n-1) sets

main = print $ S.size $snd$countF2' 10000000 iniSets

