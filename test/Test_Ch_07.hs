module Test_Ch_07 (ch_07Suite_Props,ch_07Suite_Units) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as SC
import Test.Tasty.HUnit

import Ch_07 


ch_07Suite_Props = testGroup "CH 7 Properties" []
ch_07Suite_Units = testGroup "Ch 7 Units" [eulersUnitTests,taylorsUnitTests,rkUnitTests]

eulersUnitTests = testGroup "Euler's Method Unit Tests" 
	[ 
	testCase "y'(t) = t+y // y(1) = 2 does it work at all?" $
		((snd.last)(eulers (\t y -> t + y) 2 1 1.1 0.05)) @?= 2.31,
	testCase "no steps returns params" $
		(eulers (\t y -> t + y) 2 1 1 0.05) @?= [(1.0,2.0)],
	testCase "A negative second param returns []" $
		(eulers (\t y -> t + y) 2 1 (-1) 0.05) @?= [],
	testCase "Truncation error" $
		(length $ (eulers (\t y -> t + y) 2 1 (1.5) 0.1)) @?= 6
	]

taylorsUnitTests = testGroup "Taylors's Method Unit Tests" 
	[ 
	testCase "y'(t) = t+y // y(1) = 2 does it work at all?" $
		((snd.last)(taylors (\t y -> t + y) 2 1 1.4 0.1) - 3.48302) `compare` 0.01 @?= LT,
	testCase "no steps returns params" $
		(taylors (\t y -> t + y) 2 1 1 0.05) @?= [(1.0,2.0)],
	testCase "A negative second param returns []" $
		(taylors (\t y -> t + y) 2 1 (-1) 0.05) @?= [],
	testCase "Truncation error" $
		(length $ (taylors (\t y -> t + y) 2 1 (1.5) 0.1)) @?= 6
	]

rkUnitTests = testGroup "Runge-Kutta 4's Method Unit Tests" 
	[ 
	testCase "y'(t) = t+y // y(1) = 2 does it work at all?" $
		abs (((snd.last)(rungeKutta4 (\t y -> t + y) 2 1 1.4 0.1)) - 3.5673) `compare` 0.001  @?= LT,
	testCase "no steps returns params" $
		(rungeKutta4 (\t y -> t + y) 2 1 1 0.05) @?= [(1.0,2.0)],
	testCase "A negative second param returns []" $
		(rungeKutta4 (\t y -> t + y) 2 1 (-1) 0.05) @?= [],
	testCase "Truncation error" $
		(length $ (rungeKutta4 (\t y -> t + y) 2 1 (1.5) 0.1)) @?= 6
	]