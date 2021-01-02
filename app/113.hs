import Data.List

e113 pwr = (e113' 0 pwr) + (e113' 1 pwr) - (pwr * 9)
e113' dec pwr = sum$map sum$map (e113'' dec) [1..pwr] where
  e113'' dec 1 = replicate 9 1
  e113'' dec p = acc (e113'' dec (p - 1)) where
    acc (h:[]) = [dec + h]
    acc (h:t)  = (h + (head n):n) where n = acc t
main = print $ e113 100 
