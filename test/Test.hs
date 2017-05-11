import DiamondKata
import Test.HUnit

assertEq s a b = TestCase (assertEqual s a b)
mkTest s xs = TestLabel s $ TestList xs

test1 = mkTest "diamondKata"

  [ assertEq "length 1"
             [ "A" ]
             (diamondKata ' ' ['A'..'A'])
             
  , assertEq "length 2"
             [ " A "
             , "B B"
             , " A "
             ]
             (diamondKata ' ' ['A'..'B'])
             
  , assertEq "length 3"
             [ "  A  "
             , " B B "
             , "C   C"
             , " B B "
             , "  A  "
             ]
             (diamondKata ' ' ['A'..'C'])

  , assertEq "string function"
             " A \nB B\n A \n"
             (diamondKataStr ['A'..'B'])
  ]


test2 = mkTest "helpers"

  [ assertEq "joinMirrored"
             [1,2,3,2,1]
             (joinMirrored [1,2,3])

  , assertEq "quarter"
             [ "   A"
             , "  B "
             , " C  "
             , "D   "
             ]
             (quarter ' ' ['A','B','C','D'])             
  ]


main :: IO ()
main = do
  runTestTT $ TestList [test1, test2]
  mempty
