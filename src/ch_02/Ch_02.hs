module Ch_02 (bisection, newton_raphson, secant, horner) where

bisection :: (Double -> Double) -- ^ function
			-> (Double, Double) -- ^ between what to values (a < b)
			-> Double 			-- ^ the error
			-> Maybe Double
bisection f (a,b) err
	| f a * f b > 0 = Nothing
	| (b-a)/2 < err = Just mid
	| f mid == 0 = Just mid
	| f a * f mid > 0 = bisection f (mid, b) err
	| f a * f mid <= 0 = bisection f (a, mid) err
	| otherwise = Nothing
		where mid = (a+b)/2

newton_raphson :: 	(Double -> Double) 		-- ^ f(x)
					-> (Double -> Double) 	-- ^ f'(x)
					-> Double 				-- ^ x_0
					-> Double 				-- ^ error tolerance
					-> Maybe Double			-- ^ Root
newton_raphson f fp x err
	| abs (f x) < abs (f xk) = Nothing
	| f x == 0 = Just x
	| f xk < err = Just xk
	| otherwise = newton_raphson f fp xk err
		where xk = x - f x / fp x

secant :: 	(Double -> Double) 	-- ^ f(x)
			-> Double 			-- ^ xk_1
			-> Double 			-- ^ xk_2
			-> Double 			-- ^ error tolerance
			-> Double			-- ^ Root
--secant' = observe "Informative name for secant" secant
secant f xk_1 xk_2 err
	| f xk_1 == 0 = xk_1
	| f xk < err = xk
	| otherwise = secant f xk xk_1 err
		where xk = xk_1 - f xk_1 * ((xk_1 - xk_2) / (f xk_1 - f xk_2))

horner :: Double -> [Double] -> Double
horner x = foldr (\a b -> a +b*x) 0
