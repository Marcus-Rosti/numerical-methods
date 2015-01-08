module Test_Ch_02 (ch_02Suite_Props,ch_02Suite_Units) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Ch_02 (bisection, newton_raphson, secant, horner)
import Data.Maybe

e :: Double
e = 2.7182818284590452353602874713527

f x = e**x + x
fp x = e**x + 1
fRoot = (-0.56714329040978387299996866221035554975381578718651250813513)

err = 10**(-10)


ch_02Suite_Props = testGroup "CH 2 Properties" [newton_raphson_props]
ch_02Suite_Units = testGroup "Ch 2 Units" [bisectionUnits]

bisectionUnits = testGroup "Bisection Unit Tests"
	[ testCase "Bisection arctan [-4.9,5.1] > err" $
		  (fromJust $ bisection atan ((-4.9),5.1) err ) `compare` (-err) @?= GT,

    testCase "Bisection arctan [-4.9,5.1] < err" $
      (fromJust $ bisection atan ((-4.9),5.1) err ) `compare` (err) @?= LT,

    testCase "Bisection arctan [1,5.1] == Nothing" $
      bisection atan (1,5.1) err @?= Nothing,

    testCase "Bisection e^x + x [-4.9,5.1] > err" $
      (fromJust $ bisection f ((-4.9),5.1) err ) `compare` (fRoot-err) @?= GT,

    testCase "Bisection e^x + x [-4.9,5.1] < err" $
      (fromJust $ bisection f ((-4.9),5.1) err ) `compare` (fRoot+err) @?= LT,

    testCase "Bisection e^x + x [0,5.1] == Nothing" $
      bisection f (0,5.1) err @?= Nothing
	]



newton_raphson_props = testGroup "Newton-Raphson Propeties - Checked by SmallCheck"
  [ SC.testProperty "e^x + x should converge from anywhere < root + err" $
      \x -> (fromJust $ newton_raphson f fp (x :: Double) err) <= fRoot+err,
    SC.testProperty "e^x + x should converge from anywhere > root - err" $
      \x -> (fromJust $ newton_raphson f fp (x :: Double) err) >= fRoot-err,
    SC.testProperty "x^2 +1 should never converge" $
      \x -> newton_raphson (\y -> y^2 + 1) (\y -> 2 * y) (x :: Double) err == Nothing
  ]

-- secant_props = testGroup "Secant Method Propeties\nChecked bySmallCheck"
--   [ SC.testProperty "e6x+x should converge from any two values" $ 
--       \x y -> secant f (x::Double) (y::Double) err <= fRoot+err,
--     SC.testProperty "e6x+x should converge from any two values" $ 
--       \x y -> secant f (x::Double) (y::Double) err >= fRoot-err 

--   ] 