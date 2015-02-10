--import Ch_02
--import Ch_03
import           Ch_06
import           Ch_07

e :: Double
e = exp 1

ode :: Double ->Double -> Double
ode t y = t+y

odeSol :: Double -> Double
odeSol t = (-t) + 4*e**(t-1) -1

strip4y :: [(Double, Double)] -> [Double]
strip4y = map snd

strip4t :: [(Double, Double)] -> [Double]
strip4t = map fst

listError :: [Double] -> [Double] -> [Double]
listError = zipWith (-)

inferH :: [Double] -> [Double]
inferH [] = []
inferH (f:s:xs) = (s-f) : inferH (s:xs)


main :: IO ()
main = do
	let f x = (-2) * x * sin(x**2)
	let antiF x = cos(x**2)
	-- let fp x = e**(sin x + x) * (cos x + 1)
	let err = 10**(-5)
	let a_0 = -100
	let b_0 = 0
	let real = antiF b_0 - antiF a_0
	let gq = adaptiveQuad gaussQuad f a_0 b_0 err
	let simps = adaptiveQuad simpsons f a_0 b_0 err
	let trap = adaptiveQuad trapazoid f a_0 b_0 err
	print $ real - gaussQuad f a_0 b_0
	print $ real - gq <= err*10
	print $ real - simps <= err*10
	print $ real - trap <= err*10
	print $ strip4y (eulers (\t y -> t + y) 2 1 1.5 0.1)
	print $ strip4y (taylors (\t y -> t + y) 2 1 1.5 0.1)
	print $ strip4y (rungeKuttaFehlberg (\t y -> t + y) 2 1 1.5 0.1 0.001)
	let rkfSols = rungeKuttaFehlberg ode 2 1 1.5 (2**(-16)) (10**(-4))
	print "RKF solutions"
	print rkfSols
	let ts = strip4t rkfSols
	print $ inferH ts
	let sols = map odeSol ts
	print "errors"
	let errors = listError sols (strip4y rkfSols)
	print $ sum errors

	--print $ rungeKuttaFehlberg (\t y -> t + y) 2 1 1.4 0.1 (10**(-3))
