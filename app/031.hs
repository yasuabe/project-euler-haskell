pe031 :: Int->[Int]->[[Int]]
pe031 n (1:[]) = [[n]]
pe031 n (u:us) = [(x:y)|x<-[0..(div n u)], y<-(pe031 (n-(x*u)) us)]

main = print $ length $ pe031 200 [200,100,50,20,10,5,2,1]
--73682

