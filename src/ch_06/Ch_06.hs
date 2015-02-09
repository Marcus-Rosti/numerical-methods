module Ch_06 (forwardDiff, centralDiff, secondDir,
			lagrange, trapazoid, simpsons,
			rienmannSums, gaussQuad, adaptiveQuad,
			integrate) where

import           Control.Parallel
import           Data.List

-- differentiation
forwardDiff :: 	(Double -> Double)	-- ^ f(x)
				-> Double 			-- ^ x_0
				-> Double 			-- ^ error
				-> Double			-- ^ f'(x_0)
forwardDiff f x0 h = (f (x0 + h) - f x0) / h

centralDiff :: (Double -> Double) -> Double -> Double -> Double
centralDiff f x0 h = (f (x0 + h) - f (x0 - h)) / (2*h)

secondDir :: (Double -> Double) -> Double -> Double -> Double
secondDir f x0 h = (f (x0 + h) - 2 * f x0 + f (x0 - h)) / h*h

-- Integration
rienmannSums :: (Double -> Double) -> Double -> Double -> Double -> Double
rienmannSums f a b n = dx * foldr (\l r -> (f a + (l * dx)) + r) 0 range
	where
		dx = (b-a)/n
		range = [0.5..n-0.5]


lagrange :: (Double -> Double) -> Double -> [Double] -> Double
lagrange f x xs = sum $ zipWith (*) (map f xs) (map lamb xs)
	where
		lamb xi = product $ map (\xj -> (x-xj)/(xi-xj)) (delete xi xs)

trapazoid :: (Double -> Double) -> Double -> Double -> Double
trapazoid f a b = (b - a) / 2 * ( f a + f b)

simpsons :: (Double -> Double) -> Double -> Double -> Double
simpsons f a b = (b-a)/6 * (f a + 4 * f ((a+b)/2) + f b)

-- 2 point Gaussian Quad
gaussQuad :: (Double -> Double) -> Double -> Double -> Double
gaussQuad f a b = (b - a) / 2 * (alpha0 * f v0 + alpha1 * f v1)
	where
		z0 = -1/ sqrt 3
		z1 = -z0
		alpha0 = 1
		alpha1 = 1
		v0 = (z0*(b-a)+a+b)/2
		v1 = (z1*(b-a)+a+b)/2

adaptiveQuad :: ((Double -> Double) -> Double -> Double -> Double)
				 -> (Double -> Double) -> Double -> Double -> Double
				 -> Double
adaptiveQuad quadRule f a b err
	| abs (mainQuad - testQuad) < (err/10) = mainQuad
	| otherwise = par n1 (pseq n2 (n1 + n2))
 		where
 			mid = (a+b)/2
 			mainQuad = quadRule f a b
 			testQuad = quadRule f a mid + quadRule f mid b
 			n1 = adaptiveQuad quadRule f a mid err
 			n2 = adaptiveQuad quadRule f mid b err

-- adaptiveQuad quadRule f a b err
-- 	| abs (mainQuad - testQuad) < (err/10) = mainQuad
-- 	| otherwise = n1 + n2
--  		where
--  			mid = (a+b)/2
--  			mainQuad = quadRule f a b
--  			testQuad = quadRule f a mid + quadRule f mid b
--  			n1 = adaptiveQuad quadRule f a mid err
--  			n2 = adaptiveQuad quadRule f mid b err

integrate :: (Double -> Double)
           -> Double -> Double -> Double -> Double
integrate = adaptiveQuad gaussQuad
