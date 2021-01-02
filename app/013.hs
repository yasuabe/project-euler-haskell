main = do cnt <- readFile "files/p13.txt"
          print $ pe013 $ lines cnt

pe013 lines = take 10 $ show $sum $ map read lines
