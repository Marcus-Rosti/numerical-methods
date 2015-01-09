--import Ch_02
--import Ch_03
import Ch_06
import Ch_07

e :: Double
e = exp 1

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
	print $ antiF b_0 - antiF a_0
	print $ eulers (\t y -> t + y) 2 1 1.6 0.1
	print $ taylors (\t y -> t + y) 2 1 1.6 0.1