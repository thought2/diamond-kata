module DiamondKata
  (diamondKata)
where

import Data.Maybe

diamondKata :: String -> [String]
diamondKata chars =
    joinMirrored
  . map joinMirrored
  . quarter
  $ chars



quarter :: String -> [String]
quarter chars =
  zipWith (placeIn width) chars offsets
  where
    width   = length chars
    offsets = reverse [0..width-1]

joinMirrored :: [a] -> [a]
joinMirrored xs =
  xs ++ tail (reverse xs)

placeIn :: Int -> Char -> Int -> String
placeIn width char pos = concat
  [ mkSpace pos
  , [char]
  , mkSpace $ width - pos - 1
  ]
  
mkSpace :: Int -> String
mkSpace n =
  replicate n ' '
