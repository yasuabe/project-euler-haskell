-- TODO: too slow. need better algorithm
f1 0  _  _  = 1
f1 n  ac lc = o + a + l
    where o = f1 (n-1) 0 lc
          a = if ac < 2 then f1 (n-1) (ac+1) lc else 0
          l = if lc ==0 then f1 (n-1) 0      1  else 0

main = print$f1 30 0 0
-- $ ./191 +RTS -K2500M -RTS
