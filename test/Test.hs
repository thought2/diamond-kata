import DiamondKata
import Test.HUnit

assertEq s a b = TestCase (assertEqual s a b)
mkTest s xs = TestLabel s $ TestList xs

test1 = mkTest "diamondKata"

  [ assertEq "length 1"
             (diamondKata ' ' ['A'..'A'])
             [ "A" ]
             
  , assertEq "length 2"
             (diamondKata ' ' ['A'..'B'])
             [ " A "
             , "B B"
             , " A "
             ]
             
  , assertEq "length 3"
             (diamondKata ' ' ['A'..'C'])
             [ "  A  "
             , " B B "
             , "C   C"
             , " B B "
             , "  A  "
             ]

  , assertEq "string function"
             (diamondKataStr ['A'..'B'])
             " A \nB B\n A \n"             
  ]


test2 = mkTest "helpers"

  [ assertEq "joinMirrored"
             (joinMirrored [1,2,3])
             [1,2,3,2,1]
             
  , assertEq "insertBehind"
             (insertBehind [0,0,0,0] 1 9)
             [0,0,9,0,0]

  , assertEq "quarter"
             (quarter ' ' ['A','B','C','D'])       
             [ "   A"
             , "  B "
             , " C  "
             , "D   "
             ]
  ]


main :: IO ()
main = do
  runTestTT $ TestList [test1, test2]
  mempty
