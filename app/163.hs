-- TODO: refactor
--http://www.mathpuzzle.com/bdalytriangles.html

tri n = div (n*(n+1)) 2
tri' = tri.floor
floor'=fromIntegral.floor
ceiling'=fromIntegral.ceiling
f1 s = sum $map (\h->tri (s+1-h))  [1..s]
f2 s=sum$map tri'$[s-2*i+1|i<-[1..b]] where b=floor'(s/2)
f3   = f1 
f4 s=sum$map tri'$[floor'(s-(ceiling' i)/3)-i+1|i<-[1..b]] where b=floor'(s*3/4)
f5 s=sum$map tri'$[floor'(s-i*4/3)+1|i<-[b,b-0.5..0.5]] where b=floor'(s*3/2)/2
f6 s =sum$map tri'$[1+floor'(s-ceiling'(2*i/3)-i)|i<-[mxw,mxw-0.5..0.5]]
    where mxw=min (f*3/2) e;d=floor'(2*(s*2/5))/2;e=floor'(s-d);f=floor' (s-e);
f7 s=sum $map tri$[s-2*i+1|i<-[1..div s 2]]
f8 s = sum $map tri$f8' where
  f8' | m3==0 = f8' 0
      | m3==1 = f8' 1
      | m3==2 = f8' 2
     where m3=mod s 3
           f8' m=filter(0<)$concat [if m== mod i 3 then [i+1,i,i-1] else [i,i,i]|i<-[0..s-1]]
f9 s = sum$[tri'$floor'(s-i/2)-i+1|i<-[1..mxw]] where mxw=floor' (s*2/3)
f10 = f1

e163 = (f1 36)
      + (f2 36)
      + (f3 36) *3
      + (f4 36) *3
      + (f5 36) *6
      + (f6 36) *6
      + (f7 36) *6
      + (f9 36) *6
      + (f8 36) *2
      + (f10 36)*6
main = print$e163
