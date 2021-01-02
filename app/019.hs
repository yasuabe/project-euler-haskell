-- In year 1900, there are 365 days. 
-- 365 = 52 * 7 + 1 and 1900/1/1 is Monday(1), so 1901/1/1 is Tuesday(2)
 
pe019 = do 
    y <-[1901..2000]
    m <-[1..12]
    return $(y, m, days y m)

pe019' []          w = []
pe019' ((y,m,d):s) w = (y,m,d,w): pe019' s (mod (d+w) 7) 

days y m | elem m [1,3,5,7,8,10,12] = 31
         | elem m [4,6,9,11]     = 30
         | isLeap y              = 29
         | otherwise             = 28

isLeap y | 0==mod y 400 = True
         | 0==mod y 100 = False
         | 0==mod y 4   = True
         | otherwise    = False

main = print
     $ length $ filter (\(y,m,d,w) -> w == 0) $ pe019' pe019 2
