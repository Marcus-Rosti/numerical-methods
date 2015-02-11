import Test.Tasty

import Test_Ch_02 (ch_02Suite_Props,ch_02Suite_Units)
import Test_Ch_06 (ch_06Suite_Props,ch_06Suite_Units)
import Test_Ch_07 (ch_07Suite_Props,ch_07Suite_Units)
import Test_Ch_09 (ch_09Suite_Props,ch_09Suite_Units)
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties,unitTests]

unitTests = testGroup "All Unit Tests" [ch_02Suite_Units, ch_06Suite_Units, ch_07Suite_Units, ch_09Suite_Units]
properties = testGroup "All Property Tests" [ch_02Suite_Props, ch_06Suite_Props]
