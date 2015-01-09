module Test_Ch_07 (ch_07Suite_Props,ch_07Suite_Units) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as SC
import Test.Tasty.HUnit

import Ch_07 


ch_07Suite_Props = testGroup "CH 7 Properties" []
ch_07Suite_Units = testGroup "Ch 7 Units" [eulersUnitTests]

eulersUnitTests = testGroup "Euler's Method Unit Tests" 
	[ 
	testCase "y'(t) = t+y // y(1) = 2 does it work at all?" $
		((snd.last)(eulers (\t y -> t + y) 2 1 1.1 0.05)) `compare` 2.31 @?= EQ,
	testCase "no steps returns params" $
		(eulers (\t y -> t + y) 2 1 1 0.05) `compare` [(1.0,2.0)] @?= EQ,
	testCase "A negative second param returns []" $
		(eulers (\t y -> t + y) 2 1 (-1) 0.05) `compare` [] @?= EQ
	]