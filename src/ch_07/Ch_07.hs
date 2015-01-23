module Ch_07 (eulers, taylors, rungeKutta4, rungeKuttaFehlberg) where

eulers :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
eulers yprime y_0 t_0 t_n h
	| t_0 == t_n = [(t_0, y_0)]
	|  t_0 <=(t_n+h) = (t_0, y_0) : eulers yprime (y_0 + h * (yprime t_0 y_0)) (t_0 + h) t_n h
	| otherwise = []

taylors :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
taylors yprime y_0 t_0 t_n h
	|  t_0 == t_n = [(t_0, y_0)]
	|  t_0 <=(t_n+h) = (t_0, y_0) : eulers yprime (y_0 + h * phi) (t_0+h) t_n h
	| otherwise = []
		where 
			phi = yprime (t_0 + h/2) (y_0 + h/2 * yprime t_0 y_0)

rungeKutta4 :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
rungeKutta4 yprime y_0 t_0 t_n h
	| t_0 == t_n = [(t_0, y_0)]
	|  t_0 <=(t_n+h) = (t_0, y_0) : rungeKutta4 yprime y_kp1 (t_0+h) t_n h 
	| otherwise = []
		where
			y_kp1 = y_0 + (h/6) * (k1 +2*k2 +2*k3+k4)
			k1 = yprime t_0 y_0
			k2 = yprime (t_0 + h/2) (y_0 + h/2 * k1)
			k3 = yprime (t_0 + h/2) (y_0 + h/2 * k2)
			k4 = yprime (t_0+h) (y_0 + h * k3)

rungeKuttaFehlberg :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> Double -> [(Double, Double)]
rungeKuttaFehlberg yprime y_0 t_0 t_n h tolerance
	| t_0 == t_n = [(t_0, y_0)]
	| t_0 > (t_n) = []
	| h > 1 = rungeKuttaFehlberg yprime y_0 t_0 t_n 1 tolerance
	| local_error <= tolerance = (t_0, y_0) : rungeKuttaFehlberg yprime y_kp1 (t_0+h) t_n hnext tolerance
	| local_error > tolerance = rungeKuttaFehlberg yprime y_0 t_0 t_n hnext tolerance
 	| otherwise = []
		where 
			local_error = (abs ((y_kp1 - y_kp1_TEST) / h))
			q =  ((0.1 * tolerance * h) / local_error) ** (1/4)
			k1 = yprime t_0 y_0 
			k2 = yprime (t_0 + h/4) (y_0 + h/4*k1)
			k3 = yprime (t_0 + 3*h/8) (y_0 + h*(3/32*k1 + 9/32*k2))
			k4 = yprime (t_0 + 12*h/13) (y_0 + h*(1932/2197*k1 - 7200/2197*k2 + 7296/2197*k2))
			k5 = yprime (t_0 + h) (y_0 + h*(439/216*k1 - 8*k1 + 3680/513*k3 - 845/4104*k4))
			k6 = yprime (t_0 + h/2) (y_0 + h*((-8)/27*k1 + 2*k2 - 3544/2565*k3 + 1859/4104*k4 - 11/40 *k5))

			y_kp1 = y_0 + h*(25/216*k1 + 1408/2565*k3 + 2197/4104*k4 - 1/5*k5)
			y_kp1_TEST = y_0 + h*(16/135*k1 + 6656/12825*k3 + 28561/56430*k4 - 9/50*k5 + 2/55*k6)

			hnext 
				| q <= 0.1 = (h/10)
				| q <= 4 && q > 0.1  = q*h
				| q > 4 = 4*h