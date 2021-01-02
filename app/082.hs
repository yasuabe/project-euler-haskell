--need refactor
--need performance tuning
import Data.Array.IO
import Data.List
import Data.Function
import Control.Monad
import Data.String.Utils
type Pos = (Int, Int)
size=80

main = do cnt<-readFile "matrix.txt"
          let  mtx= concatMap (row::String->[Int])$lines cnt
          print$ length mtx
	  e82 mtx>>=print
       where row s = map read$split "," s

e82 mtx= do
    vals <- mapM (f mtx) [1..size]
    return$minimum vals

f mtx row = do
    arr <- newArray (0,size^2-1) 1000000:: IO(IOArray Int Int)
    let pv@(pos,val) = ((row,1), valAt mtx (row,1))
    writeArray arr (posToIdx pos) val
    f0 mtx [pv] arr

f0 mtx pvs arr = do
    nexts <- ioa
    if null nexts then shortest
                  else f0 mtx nexts arr
    where ioa = f1 mtx pvs arr
          shortest = (\iob->do {rs<-iob;return$minimum rs})$mapM (\row->sumAt arr (row,size)) [1..size]
f1 mtx pvs arr = do
    pvs' <- mapM(f2 mtx arr) pvs
    let pvs2 = groupBy (\a b->(on (==) fst) a b)$sortBy (\a b->(on compare fst) a b)$concat pvs'
    let pvs'' =map (\pvs'''->minimumBy (\pv1 pv2->(on compare snd) pv1 pv2) pvs''') pvs2
    mapM (\(pos,val)->writeArray arr (posToIdx pos) val) pvs''
    return pvs''

f2 ::[Int]->IOArray Int Int->(Pos,Int)->IO [(Pos,Int)]
f2 mtx arr pv=
    let nexts = f3 mtx pv in
    filterM (\next@(pos,val)-> do { val'<- sumAt arr pos; return$val < val'}) nexts

f3 mtx pv@(pos@(row,col),val)
    | col==size = []
    | otherwise = map add nexts' 
    where nexts=[(row-1,col),(row+1,col),(row,col+1)]
          nexts'=filter (\(row,col)->1<=row&&row<=size&&col<=size) nexts
          add next = (next,val+val') where val' = valAt mtx next

valAt mtx pos =mtx!!(posToIdx pos)
sumAt arr pos@(row,col) = readArray arr (posToIdx pos) 
posToIdx (row,col) = (row-1)*size+(col-1)
