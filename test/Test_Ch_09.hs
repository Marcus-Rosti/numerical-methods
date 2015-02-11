module Test_Ch_09 (ch_09Suite_Props,ch_09Suite_Units) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as SC
import Test.Tasty.HUnit

import Ch_09 (boundedMin)

ch_09Suite_Props = testGroup "CH 9 Properties" []
ch_09Suite_Units = testGroup "Ch 9 Units" [boundedMinUnits]

-- boundedMinProperties = testGroup "Golden section search properties"
--   [
--     SC.testProperty "f(x) = x^2+x has a min at -1/2"
--   ]

err :: Double
err = 2**(-6)

boundedMinUnits = testGroup "Golden section search unit tests"
  [
    testCase "f(x) = x^2+x has a min at -1/2 between -1 and 1 <err" $
      boundedMin (\x -> x*x+x) (-1) 1 err `compare` ((-1)/2 + err) @?= LT,
    testCase "f(x) = x^2+x has a min at -1/2 between -1 and 1 >err" $
      boundedMin (\x -> x*x+x) (-1) 1 err `compare` ((-1)/2 - err) @?= GT,
    testCase "f(x) = x^2+x has a min at -1/2 between -10 and 10 <err" $
      boundedMin (\x -> x*x+x) (-10) 10 err `compare` ((-1)/2 + err) @?= LT,
    testCase "f(x) = x^2+x has a min at -1/2 between -10 and 10 >err" $
      boundedMin (\x -> x*x+x) (-10) 10 err `compare` ((-1)/2 - err) @?= GT,
    testCase "f(x) = x^2+x has a min at -1/2 between -10 and 0 <err" $
      boundedMin (\x -> x*x+x) (-10) 0 err `compare` ((-1)/2 + err) @?= LT,
    testCase "f(x) = x^2+x has a min at -1/2 between -10 and 0 >err" $
      boundedMin (\x -> x*x+x) (-10) 0 err `compare` ((-1)/2 - err) @?= GT,
    testCase "f(x) = x^2+x has a min at 0 between 0 and 10 <err" $
      boundedMin (\x -> x*x+x) 0 10 err `compare` (0 + err) @?= LT,
    testCase "f(x) = x^2+x has a min at 0 between 0 and 10 >err" $
      boundedMin (\x -> x*x+x) 0 10 err `compare` (0 - err) @?= GT
  ]
