module Main where

import TH
import Data.Map as M
import Days (days)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  case lookupMax days of
    Nothing -> putStrLn "no days found!"
    Just (i, f) -> do
      raw <- TIO.readFile $ "input/" ++ show i ++ ".text"
      f raw
