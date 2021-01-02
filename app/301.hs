-- TODO: over 1 minute
import Data.Bits

nimSum n = xor (xor n (n*2)) (n*3)
solve lmt = length $ filter (0==) $ map nimSum [1..lmt]
main = print $ solve (2^30::Integer)
