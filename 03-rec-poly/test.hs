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

localMaximaTests = test
  [ "localMaximaTest1" ~: "localMaxima []" ~: [] ~=? localMaxima []
  , "localMaximaTest3" ~: "localMaxima [2,9,5,6,1]" ~: [9,6] ~=? localMaxima [2,9,5,6,1]
  , "localMaximaTest4" ~: "localMaxima [2,3,4,1,5]" ~: [4] ~=? localMaxima [2,3,4,1,5]
  , "localMaximaTest5" ~: "localMaxima [1,2,3,4,5]" ~: [] ~=? localMaxima [1,2,3,4,5]
  ]

main = do
  runTestTT skipsTests
  runTestTT localMaximaTests
