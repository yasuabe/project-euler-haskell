-- TODO: manual intervention
f x = (fromIntegral$floor (2**(30.403243784-x^2))) * ((fromIntegral 10)^^(-9))
u 0 = -1
u n = (f.u) (n-1)

--ghci> mapM_ (print.u) [1..]
-- ...
-- 0.681175878
-- 1.029461839
-- 0.681175878
-- 1.029461839
-- convergence over around 500
--ghci> 0.681175878 + 1.029461839
--1.710637717
