import Data.Char

main = print
     $ maximum
     $ concatMap f [1..99]
     where f b   = map (g b) [1..99]
           g b a = sum $ map digitToInt $ show (a^b)
