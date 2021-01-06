import Data.Numbers.Primes (primeFactors)
import qualified Data.List as List
import Data.List (group)
import qualified Data.IntMap as Map
import Data.IntMap(IntMap, keys, fromList, empty, foldrWithKey, unionWith, (!))
import Data.Bits
import Data.Tuple.Extra

type PrimeFactors = Map.IntMap Int

modulo :: Int
modulo = 1000000007

modfunc f a b = mod (f a b) modulo
modplus = modfunc (+)
modmult = modfunc (*)

modpow b e = f b e 1 where
  f b e r = if e > 0
            then f (modmult b b) (shiftR e 1) (if (e .&. 1) == 1 then modmult r b else r)
            else mod r modulo

modsumpow b e = let num  = ((modpow b (e + 1)) - 1)
                    num' = if num < 0 then num + modulo else num in
                  modmult num' $ modpow (b - 1) (modulo - 2)

factorsTable = (empty : ) $ map (fromList . factorize) [1..20000]
  where factorize = map (head &&& length) . group . primeFactors

streamOfB :: [PrimeFactors]
streamOfB = map (\(_,b,_)->b) $ iterate (uncurry3 step) (1, empty, empty)
  where
    union e fs acc  = unionWith (+) acc $ Map.map (*e) fs
    step n cur fact = let n'   = n + 1
                          pfs  = factorsTable !! n'
                          next = union (-1) fact $ union n pfs cur in
                        (n', next, union 1 pfs fact)

streamOfD :: [Int]
streamOfD = map calcD streamOfB
  where calcD = foldrWithKey f 1 where f b e acc = modmult (modsumpow b e) acc

calcS :: Int -> Int
calcS n = (!!n) $ scanl modplus 0 streamOfD

main = print $ calcS 20000