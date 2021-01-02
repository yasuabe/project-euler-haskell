toHex :: Integer->String
toHex 0 = "0"
toHex n = reverse $f n where
    f 0 = []
    f n = "0123456789ABCDEF"!!(fromInteger (mod n 16)):f (div n 16)
    toDecimal 'F' = 15

e162 n = total-a-b-c+ab+ac+bc-abc where
    total = 15*16^n
    a = 15*15^n   -- without 0
    b = 14*15^n   -- without 1
    c = 14*15^n   -- without A
    ab = 14*14^n  -- without 0 and 1
    ac = 14*14^n  -- without 0 and A
    bc = 13*14^n  -- without 1 and A
    abc = 13*13^n -- without 0, 1 and A
main = print$toHex$sum$map e162 [2..15]

--ghci> :main
