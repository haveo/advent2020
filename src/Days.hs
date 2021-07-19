{-# LANGUAGE TemplateHaskell #-}

module Days where

import qualified Days.Day01
import qualified Days.Day02
import qualified Days.Day03
import qualified Days.Day04
import qualified Days.Day05
import qualified Days.Day06
import qualified Days.Day07
import qualified Days.Day08
import qualified Days.Day09
import qualified Days.Day10
import qualified Days.Day11
import qualified Days.Day12
import qualified Days.Day13
import qualified Days.Day14
import qualified Days.Day15
import qualified Days.Day16
import qualified Days.Day17
import qualified Days.Day18
import qualified Days.Day19
import qualified Days.Day20
import qualified Days.Day21
import qualified Days.Day22
import qualified Days.Day23
import qualified Days.Day24
import qualified Days.Day25

import Data.Map
import qualified TH
import Text.Megaparsec
import Data.Text

days :: Map Int (Text -> IO ())
days = $(TH.getAllDays)
