import Control.Monad
pe004 = maximum $ do 
    x <- [100..999]
    y <- [100..999]
    guard $ isPalindromic $ x * y
    return $ x * y
  where
    isPalindromic n = let s = show n
                      in s == reverse s
main = print pe004

-- $ time ./004 +RTS -K2500M -RTS
-- 906609
--
-- real	0m0.216s
-- user	0m0.212s
-- sys	0m0.002s
