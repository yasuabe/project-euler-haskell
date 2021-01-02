-- p0->p1->p3
e144 (x0, y0) (x1, y1) = calcP3 (x1,y1) $calcP2 (x0,y0) (x1,y1)

-- p0->p1->p2
calcP2 (x0, y0) (x1, y1) = extend (x0-m',y0-m'*m) where
    m = -(4*x1/y1)
    c = -(x1+m*y1)
    m' = (x0+m*y0+c)/(1+m^2)
    extend (xe, ye) = (x0+2*(xe-x0),y0+2*(ye-y0))

-- p1->p2->p3
calcP3 (x1,y1) (x2,y2) =
    let x3 = exclude x1$solve$transform (x1,y1) (x2,y2)
    in (x3, getY3 (x1,y1) (x2,y2) x3) where
    exclude x1 (xa, xb) = if abs (xa-x1)<abs (xb-x1) then xb else xa
    getY3 (x1,y1) (x2,y2) x3 = (y1-y2)/(x1-x2)*(x3-x1)+y1

-- (y-y1)=(y1-y2)/(x1-x2)*(x-x1) -> <- 4x^2+y^2=100
transform (x1,y1) (x2,y2) = (4+m^2, 2*m*n, n^2-100) where
    m = (y1-y2)/(x1-x2)
    n = -m*x1+y1

-- ax^2 + bx + c -> x=(-b(+/-)sqrt (b^2-4ac))/2a
solve (a,b,c) = (((-b)-d)/(2*a),((-b)+d)/(2*a)) where
    d = sqrt (b^2-4*a*c)

main = print $length
    $ takeWhile (\(x,y)->not (abs x < 0.01&&0<y))$tail$rep where
    rep = (0,10.1):(1.4,-9.6):zipWith e144 rep (tail rep)

