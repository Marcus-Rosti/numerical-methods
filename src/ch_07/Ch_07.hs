module Ch_07 (eulers) where

eulers yprime y_0 t_0 t_n h
	| t_0 <= t_n = (t_0, y_0) : eulers yprime (y_0 + h * (yprime t_0 y_0)) (t_0 + h) t_n h
	| otherwise = []