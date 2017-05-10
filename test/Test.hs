module Tests where

import DiamondKata

assertions :: [Bool]
assertions =
  [ diamondKata ['A'..'A'] ==
      [ "A"
      ]
  , diamondKata ['A'..'B'] ==
      [ " A "
      , "B B"
      , " A "
      ]
  , diamondKata ['A'..'C'] ==
      [ "  A  "
      , " B B "
      , "C   C"
      , " B B "
      , "  A  "
      ]
   , diamondKata ['A'..'D'] ==
      [ "   A   "
      , "  B B  " 
      , " C   C " 
      , "D     D" 
      , " C   C "
      , "  B B  "
      , "   A   "
      ]
  ]

