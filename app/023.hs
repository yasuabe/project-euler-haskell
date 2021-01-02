import qualified Data.Set as S

limit      = 28123
wholeTotal = limit * (limit + 1) `div` 2

insert e s    = if S.member e s then s else S.insert e s
sumElements s = sum $ S.toList s

isAbundant n = n < divSum n 1 where
  divSum n d  
    | n < d * 2    = 0
    | mod n d == 0 = d + next
    | otherwise    = next
    where next = divSum n (d+1)

collect (_:[])     sums = sums
collect as@(ah:at) sums = collect at $ updateSum ah as sums

updateSum abun (ah:at) sums 
  | null at         = insertIfNeed
  | limit < abunSum = sums
  | otherwise       = updateSum abun at insertIfNeed
  where
    abunSum      = abun + ah
    insertIfNeed = insert abunSum sums

abundants = filter isAbundant [1..limit]
abunTotal = sumElements $ collect abundants S.empty

main = do print $ wholeTotal - abunTotal

-- $ ghc -O2 023/023.hs 
-- [1 of 1] Compiling Main             ( 023/023.hs, 023/023.o )
-- Linking 023/023 ...
-- $ time 023/023 +RTS 
-- 4179871
-- 
-- real	0m9.933s
-- user	0m9.901s
-- sys	0m0.020s

