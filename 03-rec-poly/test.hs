import Data.Char
import Control.Monad
import System.IO
import System.Directory
import Test.HUnit

import Golf

skipsTests = test
   [ "skipsTest1" ~: "skips [1]" ~: [[1]] ~=? skips [1]
   , "skipsTest2" ~: "skips [True, False]" ~: [[True, False], [False]] ~=? skips [True,False]
   , "skipsTest3" ~: "skips [1, 2, 3]" ~: [[1, 2, 3], [2], [3]] ~=? skips [1, 2, 3]
   , "skipsTest4" ~: "skips [1, 2, 3, 4]" ~: [[1, 2, 3, 4], [2, 4], [3], [4]] ~=? skips [1, 2, 3, 4]
   ]

main = runTestTT skipsTests
