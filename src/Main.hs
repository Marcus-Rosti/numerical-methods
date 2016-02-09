--import Ch_02
--import Ch_03
import           Ch_06
import           Ch_07
import           Ch_09

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

-- TODO there's something wrong here
inferH :: [Double] -> [Double]
inferH [] = []
inferH [_,_] = []
inferH (f:s:xs) = (s-f) : inferH (s:xs)
inferH [_] = []

main :: IO ()
main = do
    let f x = (-2) * x * sin(x**2)
    let g x = (x ** 2) * sin (x+1) + x **2
    let antiF x = cos(x**2)
    -- let fp x = e**(sin x + x) * (cos x + 1)
    let err = 2**(-8)
    let a_0 = -100
    let b_0 = 0
    let c_0 = (-1000)
    let d_0 = 1000
    let real = antiF b_0 - antiF a_0
    let gq = adaptiveQuad gaussQuad f a_0 b_0 err
    let simps = adaptiveQuad simpsons f a_0 b_0 err
    let trap = adaptiveQuad trapazoid f a_0 b_0 err
    print real
    print gq
    print simps
    print simps
    print trap
    print (integrate g c_0 d_0)
