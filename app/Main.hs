module Main where

import Data.Bool
import Data.Either
import System.Environment
import DiamondKata

type Err = String

usage       = unlines
  [ "Usage: diamond-kata char char"
  , "Example: diamond-kata A D"
  ]
  
errArgN     = "Wrong number of arguments."
errArgType  = "Arguments must be single characters."
errArgOrder = "First argument must be lower than or equal to the second."

errorMsg msg = unlines [ msg, "", usage ]

parseArgs :: [String] -> Either Err [Char]
parseArgs args =
      checkNr args
  >>= checkLength
  >>= checkOrd

check :: (a -> Bool)
      -> (a -> b)
      -> Err
      -> a
      -> Either Err b
check cond postFn errMsg arg =
  bool (Left errMsg)
       (Right (postFn arg))
       (cond arg)

checkNr :: [String] -> Either Err [String]
checkNr = check
  ((==2) . length)
  id
  errArgN

checkLength :: [String] -> Either Err [Char]
checkLength = check
  (all (==1) . map length)
  (map head)
  errArgType

checkOrd :: [Char] -> Either Err [Char]
checkOrd = check
  (\xs -> case xs of
      [a,b] | a <= b -> True
      otherwise     -> False)
  (\[a,b] -> [a..b])
  errArgOrder

run :: [String] -> IO ()
run args =
    putStrLn
  $ either errorMsg diamondKataStr (parseArgs args)

main :: IO ()
main = do
  args <- getArgs
  run args

