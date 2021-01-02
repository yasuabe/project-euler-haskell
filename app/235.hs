lmt = -600000000000

u k r = (fromIntegral(900-3*k))*r^(k-1)

s r = floor$sum$map (\k->u k r) [1..5000]

f 0 r _  = r
f d r df 
  | s (r+df) <= lmt = f (d-1) r (df/10)
  | otherwise       = f d (r+df) df

main = print $ round' (f 13 1.0 0.1) 12 where
  round' f w   = (fromIntegral $ round $ f * shft) / shft
    where shft = fromIntegral $ 10^w
