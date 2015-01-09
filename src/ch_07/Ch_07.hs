module Ch_07 (eulers, taylors) where

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