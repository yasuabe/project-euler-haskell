-- TODO: need better algorithm
-- TODO: refactor
-- TODO: too slow
import Data.List
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Base(unsafeRead, unsafeWrite, unsafeAt)
import Control.Monad.ST
import Data.STRef
import qualified Data.Map as M
splits ls = (ls,[]): f ls where
    f []    = []
    f (h:t) = (t,[h]):(map (\(a,b)->(a,h:b))$f t)
c l []    = if 0 == l then [([],[])] else []
c l (h:t) = [(i1++i3, t1++t3)|(i1,t1)<-splits h, let l'=length i1, l'<=l, (i3,t3)<-c (l-l') t]

c1 _ 0 l1 l2       = [l1 ++ l2]
c1 _ c l1 (h:[])   | h< c = []
                   | h==c = [l1]
                   | otherwise = [l1 ++ [h-c]]
c1 p c l1 l2@(h:t) = concatMap (\b->c1 (p+b) (c-b) (newL b) t)[0..(min c h)] 
    where newL b= if h==b then l1 else l1++[h-b]

c2 _ 0 l1 _        = [l1]
c2 _ c l1 (h:[])   | h< c = []
                   | h==c = [l1++[h]]
                   | otherwise = [l1 ++ [c]]
c2 p c l1 l2@(h:t) = concatMap (\b->c2 (p+b) (c-b) (newL b) t)[0..(min c h)] 
    where newL b= if 0==b then l1 else l1++[b]

c2' c ls = c2 0 c [] ls

c3 ls [] = length ls
c3 ls (h:t)= c3 (concatMap (\l->c1 0 h [] l) ls) t

f1 lm 0 = [[]]
f1 lm n = concatMap (\m->map (\ls->m:ls) $f1 m (n-m)) [1..(min lm n)]

f2 n = let ls = f1 n n in map (\l->map length$group l) ls
f2' n = map (\ls->(head ls,length ls))$group$sort$map sort$f2 n

f5 ls1 ls2
    = sum
    $ concatMap (\n->map (\l->c3 (c2' n ls1) l) $c2' n ls2) [0..(min len1 len2)]
    where len1 = sum ls1
          len2 = sum ls2

f6 bc wc = runST $ do
    r <- newSTRef M.empty
    l <- mapM (\b->mapM (\w-> f7 b w r) $f2 bc) $f2 wc
    return $ sum $ concat l
  where
    f7 b w r = do m <- readSTRef r
                  case M.lookup (b,w) m of
                      Just e  -> return e
                      Nothing -> case M.lookup (w,b) m of
                                      Just e'  -> return e'
                                      Nothing -> do let e5 = f5 b w
                                                        m' = M.insert (b,w) e5 m --mM.insert (w,b) e5 $M.insert (b,w) e5 m
                                                    writeSTRef r m'
                                                    return e5

--f8 b w = sum$concatMap (\(bl,bc)->map (\(wl,wc)-> bc*wc*(f5 bl wl)) $f2' w) $f2' b
f8 b w = sum$concatMap (\(bl,bc)->map (\(wl,wc)-> bc*wc*((+) (length bl) (length wl))) $f2' w) $f2' b

main = print $ f6 40 40 -- sum$concatMap (\blacks->map (\whites-> f5 blacks whites) $f2 60) $f2 40
