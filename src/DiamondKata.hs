module DiamondKata
where

diamondKataStr :: String -> String
diamondKataStr xs =
  unlines $ diamondKata ' ' xs

diamondKata :: a -> [a] -> [[a]]
diamondKata x =
    joinMirrored
  . map joinMirrored
  . quarter x

quarter :: a -> [a] -> [[a]]
quarter empty xs =
  zipWith (insertBehind empties) offsets xs
  where
    width'  = length xs - 1
    empties = replicate width' empty
    offsets = reverse [0..width']

insertBehind :: [a] -> Int -> a -> [a]
insertBehind xs i x =
  concat [as, [x], bs]
  where
    (as,bs) = splitAt i xs

joinMirrored :: [a] -> [a]
joinMirrored xs =
  xs ++ tail (reverse xs)
