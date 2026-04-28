import Test.Tasty

import Variant
import EADT
import EGADT

main :: IO ()
main = defaultMain $ testGroup "utils-variant"
  [ testsVariant
  , testsEADT
  , testsEGADT
  ]
