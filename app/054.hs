--refactor
import Data.Char
import Data.List
import Data.Maybe

type Card = (Int,Char)
type Rank = (Int, [Int])

toCard (c1:c2:[])=(val,c2) where
  val | c1=='T' = 10
      | c1=='J' = 11
      | c1=='Q' = 12
      | c1=='K' = 13
      | c1=='A' = 14
      | otherwise = digitToInt c1
findRank =   findRoyalFlush 
         >-> findStraightFlush
         >-> findFourCards 
         >-> findFullHouse
         >-> findFlush
         >-> findStraight
         >-> findThreeCards
         >-> findTwoPair
         >-> findOnePair
         >-> findNoPair
    where (>->) a b = (\cs->if isNothing (a cs) then b cs else a cs)
findNoPair,findStraight,findFlush,findFullHouse ::[Card]->Maybe Rank

findNoPair cards = Just (0, reverse$sort$map fst cards)

findOnePair cards  
    | isOnePair = Just (1, (fst(g!!0!!0)):(reverse$sort$map fst$concat (tail g)))
    | otherwise = Nothing
    where isOnePair = 2==length (g!!0)
          g         = groupByVal cards

findTwoPair cards
    | isTwoPair  = Just (2, (max (f 0)(f 1)):(min (f 0) (f 1)):[f 2])
    | otherwise  = Nothing
    where isTwoPair = (\l->3==length l && length (l!!0)==2 && length (l!!1)==2) g
          g         = groupByVal cards
          f i       = fst$head (g!!i)

findThreeCards cards
    | isThreeCards     = Just (3, (fst$head(g!!0)):(reverse$sort$map fst$concat$tail g))
    | otherwise        = Nothing
    where isThreeCards = (\l->3==length l && length (l!!0)==3) g
          g            = groupByVal cards
          
findStraight cards
    | isStraight cards = Just (4, sorted)
    | otherwise  = Nothing
    where sorted = sortedVal cards

sortedVal cards  =  reverse$sort$ map fst cards
isStraight cards = all(==1)$zipWith (-) sorted (tail sorted)
    where sorted = sortedVal cards 

findFlush cards
    | isFlush cards = Just (5, reverse$sort$ map fst cards)
    | otherwise     = Nothing

isFlush cards = ((==1).length)$groupBy eqSuit cards

findFullHouse cards
    | isFullHouse = Just (6, [(fst.head) (grouped!!0)])
    | otherwise = Nothing
    where isFullHouse = (\l->2==length l && length (l!!0)==3 && length(l!!1)==2) grouped
          grouped     = groupByVal cards

findFourCards cards
    | isFullHouse = Just (7, [(fst.head) (grouped!!0)])
    | otherwise = Nothing
    where isFullHouse = (\l->2==length l && length (l!!0)==4) grouped
          grouped     = groupByVal cards

findStraightFlush cards
    | isStraightFlush = Just (8, sortedVal cards)
    | otherwise = Nothing
    where isStraightFlush = (isFlush cards) && (isStraight cards)
          
findRoyalFlush cards
    | isRoyalFlush = Just (9, sorted)
    | otherwise = Nothing
    where isRoyalFlush = (14==sorted!!0) && (isFlush cards) && (isStraight cards)
          sorted = sortedVal cards
          
compSuit a b = compare (snd a) (snd b)
eqSuit a b   = (snd a)==(snd b)
eqVal  a b   = (fst a)==(fst b)
groupByVal cards  = reverse
                  $ sortBy (\a b->compare (length a) (length b))
                  $ groupBy eqVal
                  $ sortBy (\a b->compare (fst a)(fst b)) cards

compareRanks (rank1,vals1) (rank2,vals2)
    | rank1 < rank2 = 2
    | rank2 < rank1 = 1
    | vals1 < vals2 = 2
    | vals2 < vals1 = 1
    | otherwise     = -1

toHands s = (\(a,b)->(map toCard a, map toCard b))$splitAt 5 cards where cards = words s
judge s = (s, rankA, rankB, compareRanks rankA rankB)
    where (a,b) = toHands s
          rankA = fromJust$findRank a
          rankB = fromJust$findRank b
pe054 = do
    cnt <- readFile "files/poker.txt"
    return $lines cnt

main = do ls <- pe054
          print $ length $ filter(\(_,_,_,d) -> d==1) $ map judge ls

