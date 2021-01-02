-- TODO: too slow
import Data.List

f [] = [[]]
f ls = [(x:xs)|x<-ls, xs<- f (delete x ls)]
lmt = 12
main = print$drop ((product [1..lmt])-10)$ g --f [1..lmt]

ls = [1..12]
g = [(a,b,c,d,e,f,g,h,i,j,k,l)|
    a<-ls,
    b<-ls,b/=a,
    c<-ls,c/=a,c/=b,
    d<-ls,d/=a,d/=b,d/=c,
    e<-ls,e/=a,e/=b,e/=c,e/=d,
    f<-ls,f/=a,f/=b,f/=c,f/=d,f/=e,
    g<-ls,g/=a,g/=b,g/=c,g/=d,g/=e,g/=f,
    h<-ls,h/=a,h/=b,h/=c,h/=d,h/=e,h/=f,h/=g,
    i<-ls,i/=a,i/=b,i/=c,i/=d,i/=e,i/=f,i/=g,i/=h,
    j<-ls,j/=a,j/=b,j/=c,j/=d,j/=e,j/=f,j/=g,j/=h,j/=i,
    k<-ls,k/=a,k/=b,k/=c,k/=d,k/=e,k/=f,k/=g,k/=h,k/=i,k/=j,
    l<-ls,l/=a,l/=b,l/=c,l/=d,l/=e,l/=f,l/=g,l/=h,l/=i,l/=j,l/=k]
    
