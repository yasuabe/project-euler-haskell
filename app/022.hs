import Data.List.Split
import Data.Char
import Data.List

e22 names = map (\(pos,val)->pos*val) $zip [1..] $map value names
value name = foldr (+) 0 $map asc name
asc ch = fromEnum ch - 64
isAbundant n = [1..(div n)]

main = do cnt<-readFile "files/names.txt" 
          print $sum$e22$sort $map (filter isAlpha) $splitOn "," cnt
