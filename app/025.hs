import Data.Digits
import Data.List

fibs = 0: 1: zipWith (+) fibs (tail fibs)
main = print $ findIndex ((==1000).length.digits 10) $ fibs

