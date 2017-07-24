{-# LANGUAGE UnicodeSyntax #-}

{-|
Module      : FChess
Description : Simple FChess Module
Copyright   : (c) Matthias Brettschneider, 2017
License     : GPL-3
Maintainer  : frosch03@frosch03.de
Stability   : experimental

The FChess module is a simple haskell module, that serves myselfe as
testing ground for developing a chess engin.

This module defines the type 'Piece' as the central data type for a
chess piece.
-}

module FChess where

import Data.List (sortBy)
import Data.Maybe (isNothing)
    
data Color
    = Black
    | White
    deriving (Eq, Show)

data Figure
    = R
    | N
    | B
    | Q
    | K
    | P
    deriving (Eq, Show)

type Position = (Int, Int)

data Piece
    =  Piece Color Figure
    deriving (Eq)

instance Show (Piece)
    where show (Piece c f)
              | c == Black && f == R = "♜"
              | c == Black && f == N = "♞"
              | c == Black && f == B = "♝"
              | c == Black && f == Q = "♛"
              | c == Black && f == K = "♚"
              | c == Black && f == P = "♟"

              | c == White && f == R = "♖"
              | c == White && f == N = "♘"
              | c == White && f == B = "♗"
              | c == White && f == Q = "♕"
              | c == White && f == K = "♔"
              | c == White && f == P = "♙"

                                       
data Field
    = Field (Int, Int) (Maybe Piece)
    deriving (Eq, Show)

showField :: Field -> [Char]
showField (Field _ (Just p)) = ' ':(show p) ++ " "
showField (Field _  Nothing) = "   "

-- | A 'Board' (for now) is just a list of 'Field's.
-- 
-- The 'Show' instance for a 'Board' generates a graphical
-- representation of the 'Board' as shown with this example:
--
-- >>> newBoard
-- ┌───┬───┬───┬───┬───┬───┬───┬───┐
-- │ ♜ │ ♞ │ ♝ │ ♛ │ ♚ │ ♝ │ ♞ │ ♜ │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │   │   │   │   │   │   │   │   │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │   │   │   │   │   │   │   │   │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │   │   │   │   │   │   │   │   │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │   │   │   │   │   │   │   │   │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │ ♖ │ ♘ │ ♗ │ ♕ │ ♔ │ ♗ │ ♘ │ ♖ │
-- └───┴───┴───┴───┴───┴───┴───┴───┘
--
-- The 'Figure's are drawn as unicode characters. 
data Board
    = Board [Field]

-- | Making 'Board' an instance of 'Show' is done by implementing the
-- 'show' method
-- 
-- ' show' renders an initial 'spacer'. This one is then followd by an
-- alternating sequence of a row and another 'spacer'.
--
-- The row is gathered from the field by filtering out all other
-- 'Field's that are not within the given row. It is then sorted
-- by it's column.
--
-- At last it's folded to a [Char].
instance Show (Board) where
     show (Board fs)
         =    (spacer 0) ++ "\n" 
           ++ concat [showRow i ++ '\n':(spacer i) ++ "\n" | i <- enumFromThenTo 8 7 1]
         where getRow i  = filter (\(Field (x, y) _) -> x == i)
                           fs
               ordRow i  = sortBy (\(Field (x1, y1) _) (Field (x2, y2) _) -> compare x1 x2)
                           $ getRow i
               showRow i = '│':(foldl (\r n -> r ++ (showField n ++ "│")) "" $ ordRow i)
               spacerT   = "┌───┬───┬───┬───┬───┬───┬───┬───┐"
               spacerM   = "├───┼───┼───┼───┼───┼───┼───┼───┤"
               spacerB   = "└───┴───┴───┴───┴───┴───┴───┴───┘"
               spacer 0  = spacerT
               spacer 1  = spacerB
               spacer _  = spacerM



-- | The 'Game' consists of a 'Board' and two lists of 'Piece's. The
-- first one contains the 'Black' 'Figure's and the second onecontains
-- the 'White' 'Figure's.
data Game
    = Game Board [Piece] [Piece]

     
newBoard :: Board
newBoard = Board
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

newGame :: Game
newGame = Game newBoard [] []

-- | 'StartPostion' denotes the 'Position' to start that move from.
type StartPostion = Position

-- | 'Range' is the distance, the 'Piece' needs to travel in order to
-- perform that move. 
type Range = Float

-- | 'Direction' is the angle in which the the move will be
-- conducted. These angles are possible:
--
-- .|---+---+---+---+---+---+---|
-- .|   |   |   |090|   |   |   |
-- .|---+---+---+---+---+---+---|
-- .|   |135|117|   |063|045|   |
-- .|---+---+---+---+---+---+---|
-- .|   |153|   |   |   |027|   |
-- .|---+---+---+---+---+---+---|
-- .|180|   |   |x x|   |   |000|
-- .|---+---+---+---+---+---+---|
-- .|   |207|   |   |   |333|   |
-- .|---+---+---+---+---+---+---|
-- .|   |225|243|   |297|315|   |
-- .|---+---+---+---+---+---+---|
-- .|   |   |   |270|   |   |   |
-- .|---+---+---+---+---+---+---|
--
-- Normal:
-- .|-----+-----+---+-----+---+-----+-----|
-- .|     |     |   | 090 |   |     |     |
-- .|-----+-----+---+-----+---+-----+-----|
-- .|     | 135 |   |     |   | 045 |     |
-- .|-----+-----+---+-----+---+-----+-----|
-- .|     |     |   |     |   |     |     |
-- .|-----+-----+---+-----+---+-----+-----|
-- .| 180 |     |   | x x |   |     | 000 |
-- .|-----+-----+---+-----+---+-----+-----|
-- .|     |     |   |     |   |     |     |
-- .|-----+-----+---+-----+---+-----+-----|
-- .|     | 225 |   |     |   | 315 |     |
-- .|-----+-----+---+-----+---+-----+-----|
-- .|     |     |   | 270 |   |     |     |
-- .|-----+-----+---+-----+---+-----+-----|
--
-- Knight:
-- .|-----+-----+-----+-----+-----|
-- .|     | 117 |     | 063 |     |
-- .|-----+-----+-----+-----+-----|
-- .| 153 |     |     |     | 027 |
-- .|-----+-----+-----+-----+-----|
-- .|     |     | x x |     |     |
-- .|-----+-----+-----+-----+-----|
-- .| 207 |     |     |     | 333 |
-- .|-----+-----+-----+-----+-----|
-- .|     | 243 |     | 297 |     |
-- .|-----+-----+-----+-----+-----|
type Direction = Int

-- | The 'Limit' describes the maximal movement range for a
-- figure. This is usually 1 for a pawn and two for a knight.
type Limit = Int

data Move
    = Move Direction (Maybe Limit)
    deriving (Eq, Show)

-- | The 'isContinuous' function returns true if the given piece moves
-- continuously over the board. This are the rook, the bishop and the
-- queen. The other pieces can move only onto a set of given fields.
isContinuous :: Piece -> Bool
isContinuous p
    = isNothing l
    where
      (Move _ l) = head $ movesOf p


-- | Provide 'movesOf' with a 'Piece' and it will return the list of
-- possible 'Move's the piece is able to make.
movesOf :: Piece -> [Move]
movesOf (Piece _     R) = [Move d Nothing  | d <- [0, 90, 180, 270]]
movesOf (Piece _     N) = [Move d (Just 2) | d <- [27, 63, 117, 153, 207, 243, 297, 333]]
movesOf (Piece _     B) = [Move d Nothing  | d <- [45, 135, 225, 315]]
movesOf (Piece _     Q) = [Move d Nothing  | d <- [0, 45, 90, 135, 180, 225, 270, 315]]
movesOf (Piece _     K) = [Move d (Just 1) | d <- [0, 45, 90, 135, 180, 225, 270, 315]]
movesOf (Piece Black P) = [Move 270 (Just 1)]
movesOf (Piece White P) = [Move  90 (Just 1)]

-- | 'attacksOf' is the pendant to 'movesOf'. Within chess the attacks
-- are the same as the moves for all figures but the pawn. For a pawn
-- the attack fields are 1 away along the two diagonales facing the
-- moving direction. This case is catched by the lower two guards.
attacksOf :: Piece -> [Move]
attacksOf p@(Piece c f)
    | f /= P
    = movesOf p

    | f == P && c == White
    = [Move d (Just 1) | d <- [45, 135]]

    | f == P && c == Black
    = [Move d (Just 1) | d <- [225, 315]]

-- | The function 'deltaByMove' calculates the deltas for each
-- coordinate and move. A delta summed together with a position will
-- give you one possible move the piece is able to make. 
deltaByMove :: Move -> (Int, Int)
deltaByMove (Move d (Just l)) = (calc cos d, calc sin d)
    where fromDeg d = d * pi / 180
          calc trig = round . ((fromIntegral l) *) . trig . fromDeg . fromIntegral
deltaByMove (Move d Nothing) = (calc cos d, calc sin d)
    where fromDeg d = d * pi / 180
          calc :: (Floating a, RealFrac a) => (a -> a) -> (Direction -> Int)
          calc trig = round . trig . fromDeg . fromIntegral

-- | With the function 'nthMoveFields' the possible positions are
-- calculated, that the given piece might move onto. 
nthMoveFields :: Int -> Position -> Piece -> [Position]
nthMoveFields i (x, y) p@(Piece c f)
    = foldl f [] deltas
      where
        deltas  = map deltaByMove $ movesOf p
        combine = (\(x',y') -> (x+(i * x'), y+(i * y')))
        valid   = (\(x,y) -> not(x > 8 || y > 8 || x < 1 || y < 1))
        f result new
            | (valid . combine) new
            = (combine new):result
  
            | otherwise
            = result

-- | The function 'nextMoveFields' calculates just the next possible
-- positions to move onto. This is used for non continuous pieces like
-- the pawn, the knight and the king.
nextMoveFields :: Position -> Piece -> [Position]
nextMoveFields
    = nthMoveFields 1

-- | With the function 'allMoveFields' all fileds a piece can move
-- onto are returned.
allMoveFields :: Position -> Piece -> [Position]
allMoveFields pos p
    | isContinuous p
    = foldl (\r i -> (nthMoveFields i pos p) ++ r) [] [1..8]

    | otherwise
    = nextMoveFields pos p
