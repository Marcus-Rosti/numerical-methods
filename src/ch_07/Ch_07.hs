module Ch_07 (eulers, taylors, rungeKutta4) where

eulers :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
eulers yprime y_0 t_0 t_n h
	| t_0 <= t_n = (t_0, y_0) : eulers yprime (y_0 + h * (yprime t_0 y_0)) (t_0 + h) t_n h
	| otherwise = []

taylors :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
taylors yprime y_0 t_0 t_n h
	| t_0 <= t_n = (t_0, y_0) : eulers yprime (y_0 + h * phi) (t_0+h) t_n h
	| otherwise = []
		where 
			phi = yprime (t_0 + h/2) (y_0 + h/2 * yprime t_0 y_0)

rungeKutta4 :: (Double -> Double -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
rungeKutta4 yprime y_0 t_0 t_n h
	| t_0 <= t_n = (t_0, y_0) : rungeKutta4 yprime y_kp1 (t_0+h) t_n h 
	| otherwise = []
		where
			y_kp1 = y_0 + (h/6) * (k1 +2*k2 +2*k3+k4)
			k1 = yprime t_0 y_0
			k2 = yprime (t_0 + h/2) (y_0 + h/2 * k1)
			k3 = yprime (t_0 + h/2) (y_0 + h/2 * k2)
			k4 = yprime (t_0+h) (y_0 + h * k3)
