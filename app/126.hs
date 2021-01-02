import Data.List
import Data.Maybe

cuboids (w,h,d,l) = surface + edge + apex
  where
    surface = (w*h + w*d + h*d)*2
    edge    = (w+h+d)*4 * (l-1)
    apex    = 8 * (if (l<3) then 0 else sum [1..(l-2)])

nextSeq (1,1,1,1) = [(1,1,1,2), (2,1,1,1)]
nextSeq (w,1,1,1) = [(w,1,1,2), (w+1,1,1,1), (w,2,1,1)]
nextSeq (w, h, d, l)
  | l> 1 || h==d = [(w,h,d,l+1)]
  | w/=h && d==1 = [(w,h,d,2), (w,h+1,d,1), (w,h,d+1,1)]
  | otherwise    = [(w,h,d,2),              (w,h,d+1,1)]

type Params = (Int,Int,Int,Int)
data Queue  = Empty | Fork (Int, [Params]) [Queue]

element (Fork e qs) = e
paramsLen queue = (length.snd.element) queue
number    queue = (fst.element) queue

enqueue :: Params -> Queue -> Queue
enqueue params queue = merge (Fork ((cuboids params), [params]) []) queue

merge :: Queue -> Queue -> Queue
merge Empty y     = y
merge x     Empty = x
merge x     y     | prio x <= prio y = join x y
                  | prio x >  prio y = join y x
 where prio (Fork (n, _) _) = n
       join (Fork e@(n1, ps1) qs1) q2@(Fork (n2, ps2) qs2)
         | n1 == n2  = Fork (n1, ps1++ps2) (qs1++qs2)
         | otherwise = Fork e (q2:qs1)

mergeAll :: [Queue] -> Queue
mergeAll []       = Empty
mergeAll [x]      = x
mergeAll (x:y:qs) = merge (merge x y) (mergeAll qs)

dequeue :: Queue -> Queue
dequeue (Fork (_, ps) qs) = foldl' (flip enqueue) (mergeAll qs)
                          $ concatMap nextSeq ps

solve n = fromJust
        $ find ((==n).paramsLen)
        $ iterate dequeue $ enqueue (1,1,1,1) Empty

main = do print$number$solve 1000
