--import Ch_02
--import Ch_03
import           Ch_06
import           Ch_07
import                     Ch_09

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
    let antiF x = cos(x**2)
    -- let fp x = e**(sin x + x) * (cos x + 1)
    let err = 10**(-5)
    let a_0 = -100
    let b_0 = 0
    let real = antiF b_0 - antiF a_0
    let gq = adaptiveQuad gaussQuad f a_0 b_0 err
    let simps = adaptiveQuad simpsons f a_0 b_0 err
    let trap = adaptiveQuad trapazoid f a_0 b_0 err
    print "ODE Examples"
    let myODE = (+)
    print $ real - gaussQuad f a_0 b_0
    print $ real - gq <= err*10
    print $ real - simps <= err*10
    print $ real - trap <= err*10
    print $ strip4y (eulers myODE 2 1 1.5 0.1)
    print $ strip4y (taylors myODE 2 1 1.5 0.1)
    print $ strip4y (rungeKuttaFehlberg myODE 2 1 1.5 0.1 0.1)
    let rkfSols = rungeKuttaFehlberg ode 2 1 1.5 (2**(-1)) (10**(-2))
    print "RKF solutions"
    print rkfSols
    let ts = strip4t rkfSols
    print $ inferH ts
    let sols = map odeSol ts
    print "errors"
    let errors = listError sols (strip4y rkfSols)
    print $ sum errors

    print $ rungeKuttaFehlberg myODE 2 1 1.4 0.1 (10**(-2))
    print "Optimization Examples"
    print $ boundedMin (\x -> x*x + x) (-10) 10 (2**(-4))
