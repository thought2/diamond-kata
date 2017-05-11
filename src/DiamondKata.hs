module DiamondKata
where

diamondKataStr :: String -> String
diamondKataStr xs =
  unlines $ diamondKata ' ' xs

diamondKata :: a -> [a] -> [[a]]
diamondKata y xs =
    joinMirrored
  . map joinMirrored
  . quarter y $ xs

joinMirrored :: [a] -> [a]
joinMirrored xs =
  xs ++ tail (reverse xs)

quarter :: a -> [a] -> [[a]]
quarter y xs =
  zipWith f xs [0..]
  where
    f x i = concat
      [ replicate (length xs-1-i) y
      , [x]
      , replicate i y
      ]
