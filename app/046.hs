import Data.Numbers.Primes

pe046 composite = any isSquare
                $ map (\n->div (composite-n) 2)
                $ takeWhile (<composite) $ primes where
    isSquare n = (round $ sqrt $ fromIntegral n)^2==n

main = print $ fst.head $ dropWhile snd 
             $ map (\c->(c, pe046 c)) $ filter (not.isPrime) [5,7..]
