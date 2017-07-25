{-|
Module      : Mechanic
Description : Here are the chess movements defined
License     : GPL-3
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}

module Data.FChess.Mechanic
    ( allMoveFields
    )
where

import Data.FChess.Datatype.Move
import Data.FChess.Datatype.Piece
import Data.FChess.Defaults

import Data.Maybe (isNothing)


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

                                    

-- | The 'isContinuous' function returns true if the given piece moves
-- continuously over the board. This are the rook, the bishop and the
-- queen. The other pieces can move only onto a set of given fields.
isContinuous :: Piece -> Bool
isContinuous p
    = isNothing l
    where
      (Move _ l) = head $ movesOf p




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
nthMoveFields :: Int -> (Int, Int) -> Piece -> [(Int, Int)]
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
nextMoveFields :: (Int, Int) -> Piece -> [(Int, Int)]
nextMoveFields
    = nthMoveFields 1

-- | With the function 'allMoveFields' all fileds a piece can move
-- onto are returned.
allMoveFields :: (Int, Int) -> Piece -> [(Int, Int)]
allMoveFields pos p
    | isContinuous p
    = foldl (\r i -> (nthMoveFields i pos p) ++ r) [] [1..8]

    | otherwise
    = nextMoveFields pos p
