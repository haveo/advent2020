{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TH where

import Language.Haskell.TH
import Formatting
import qualified Data.Map as M
import qualified Data.Text as T

getAllDays :: Q Exp
getAllDays = do
  xs <- go 1
  [| M.fromList $(return $ ListE xs) |]
  where
    go :: Int -> Q [Exp]
    go n = do
      res <- lookupValueName (name n)
      maybe (return []) (\name -> do
        xs <- go (n+1)
        x <- [| (n, $(return $ VarE name)) |]
        return $ x : xs) res
    name n = T.unpack $ sformat ("Days.Day" % left 2 '0' % ".main") n
