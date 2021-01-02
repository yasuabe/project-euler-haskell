import Data.List
import Data.Maybe
import Data.String.Utils
type Pos = (Int,Int)
type State = [[Maybe Int]]
type Matrix = [[Int]]


e83 [] state _  = state
e83 stack state matrix=  e83 stack' state' matrix
    where (stack', state')= move stack state matrix

move stack state matrix = updatePos (minPos, rest) state matrix
       where minPos = minimumBy (\p1 p2->compare (val p1) (val p2)) stack
             rest   = delete minPos stack
             val  (r,c) = fromJust $ state!!r!!c

updatePos :: (Pos, [Pos])->State->Matrix->([Pos],State)
updatePos (minPos@(r,c), rest) state matrix = 
              let emptyPos = findEmptyPos minPos 
              in  (emptyPos++rest, update (fromJust (state!!r!!c)) emptyPos state)
    where
        findEmptyPos minPos@(row, col) = filter isEmpty (adj minPos)
        isEmpty (row, col) = isNothing $state!!row!!col
        adj (row, col)= filter isProper
            $ map (\(ros, cos)->(row+ros, col+cos)) [(0,-1),(-1,0),(0,1),(1,0)]
        isProper (r,c) = 0<=r&&0<=c&&r<=lmt&&c<=lmt where lmt=(length matrix)-1
        update _      []      state = state
        update curVal (ph:pt) state = update curVal pt state'
            where
                state' = update' ph (Just (curVal+(valAt ph))) state
                valAt (r,c) = matrix!!r!!c
 

update' :: Pos->Maybe Int->State->State
update' (r, c) v state =
    let newRow = updateList c v (state!!r)
    in  updateList r newRow state

updateList i v l = f ++ (v:bs) where (f,(b:bs)) = splitAt i l

main = do cnt<-readFile "files/matrix.txt"
          let  matrix= map (row::String->[Int])$lines cnt
               initState = update' (0,0) (Just (matrix!!0!!0))
                                   [[Nothing|x<-[1..80]]|x<-[1..80]]
          print $((!!79).(!!79)) $e83 [(0,0)] initState matrix
       where row s = map read$split "," s
