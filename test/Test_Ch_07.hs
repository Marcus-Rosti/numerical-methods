module Test_Ch_07 (ch_07Suite_Props,ch_07Suite_Units) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as SC
import Test.Tasty.HUnit

import Ch_07 


ch_07Suite_Props = testGroup "CH 7 Properties" []
ch_07Suite_Units = testGroup "Ch 7 Units" []