import Data.Array

size = 2000
lfg4M = take (size^2) lfg where
    lfg = head55++zipWith (\e24 e55->s' (e24+e55+1000000)) lfg (drop 31 lfg)
        where head55 = map (\k->s' (100003-200003*k+300007*k^3)) [1..55]
              s' k'  = mod k' 1000000 - 500000

hrz = hrz' lfg4M
    where hrz' [] = []
          hrz' l  = h: hrz' t
              where (h,t) = splitAt size l
vrt = vrt' (replicate size []) lfg4M
    where vrt' sums [] = sums
          vrt' sums l  = vrt' (zipWith ((:)) h sums) t
            where (h,t) = splitAt size l
dgn  = dgn' [] id      lfg4M
adgn = dgn' [] reverse lfg4M

dgn' ls rev r | null newRows = []
              | otherwise    = sum: dgn' tails' rev t
    where (h,t)   = splitAt size r
          newRows = filter (not.null) ((rev h):ls)
          sum     = map head newRows
          tails'  = map tail newRows

maxSum nums = maxSum' 0 0 nums where
    maxSum' mx _   [] = mx
    maxSum' mx pre (h:t)
        | cur<0     = maxSum' mx  0   t
        | mx<cur    = maxSum' cur cur t 
        | otherwise = maxSum' mx  cur t
        where cur = pre + h

e149 = maximum$map maxSum$ hrz ++ vrt ++ dgn ++ adgn

main = print e149
