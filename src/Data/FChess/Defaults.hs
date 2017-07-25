{-|
Module      : Defaults
Description : Here is a standard chess geme defined
License     : GPL-3
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}

module Data.FChess.Defaults
    ( defaultBoard
    )
where

import Data.FChess.Datatype.Board
import Data.FChess.Datatype.Piece

defaultBoard :: Board
defaultBoard = Board
           $  whiteBaseline
           ++ whitePawnline
           ++ emptyLines
           ++ blackPawnline
           ++ blackBaseline
    where baselineFields i = map (\x -> (i, x)) [1..8]
          baselineFigures = [R, N, B, Q, K, B, N, R]
          pawnlineFigures = replicate 8 P
          whiteBaseline = map (\(f,p) -> Field f (Just p)) $ zip (baselineFields 1) (map (\x -> Piece White x) baselineFigures)
          whitePawnline = map (\(f,p) -> Field f (Just p)) $ zip (baselineFields 2) (map (\x -> Piece White x) pawnlineFigures)
          blackBaseline = map (\(f,p) -> Field f (Just p)) $ zip (baselineFields 8) (map (\x -> Piece Black x) baselineFigures)
          blackPawnline = map (\(f,p) -> Field f (Just p)) $ zip (baselineFields 7) (map (\x -> Piece Black x) pawnlineFigures)
          emptyLines = map (\f -> Field f Nothing) [(i,j) | i <- [3..6], j <- [1..8]]
