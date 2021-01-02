import Data.List

funcs = [
    (\n->div (n*(n+1)) 2),
    (\n->n*n),
    (\n->div (n*(3*n-1)) 2),
    (\n->n*(2*n-1)),
    (\n->div (n*(5*n-3)) 2),
    (\n->n*(3*n-2))]
numList = map (\f->takeWhile (<10000)$dropWhile (<1000)$map f [1..]) funcs
numListPerm = map (++[last numList]) (permutations (take 5 numList))
e61 nums = 
    filter  (\s->div (head s) 100 == mod (s!!5) 100) $ e61' nums where
    e61' nums@(nh:[]) = [[cur]|cur<-nh]
    e61' nums@(nh:nt) = [(cur:nexts)|cur<-nh, nexts<-e61' nt,
                                     mod cur 100==div (head nexts) 100]

main = print
     $ sum
     $ head
     $ concatMap e61 numListPerm

-- $ ./61 +RTS -K500M -RTS
-- 28684
