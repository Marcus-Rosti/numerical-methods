cubicSplineInterp x [xjp2, xjp1, xj, xjm1, xjm2]
	| x >  xjp2 = 0
	| x >= xjp1 = (1/(6*h^3)) * (xjp2 -x)^3
	| x >= xj = 1/6 + (1/(2*h)) *(xjp1-x) + (1/(2*h^2)) *(xjp1-x)^2 - (1/(2*h^3)) *(xjp1-x)^3
	| x >= xjm1 =  2/3 - 1/(h^2) *(x-xj)^2 - 1/(2*h^3) *(x-xj)^3
	| x >= xjm2 = 1/(6*h^3) * (x-xjm2)^3
	| otherwise = 0
	where
		h = xjp1 - xj
