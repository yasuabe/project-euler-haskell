-- TODO: refactor
-- TODO: too slow
f n= (+(if m^2==n then 1 else 0))$(*2)$length$ filter (\l->0==mod n l) [1..(m-1)] where m=ceiling$sqrt$fromIntegral n
l = map f [1..10000001]
e179=sum$zipWith (\a b->if a==b then 1 else 0) l $tail l
main=print e179
--$ ./179 +RTS -K1500M -RTS

