module PositionEvaluation (evaluate) where

import BoardFramework
import qualified Data.IntMap as IM 


--takes a position, and calculates the rating
evaluate :: Position -> Float
evaluate pos = evaluateMaterial pos
{-
material, attacked pieces, doubled pawns, knight on rim, bishop pair. Kings being captured at -inf? will prevent having to check if can be captured 
when calculating pseudo-legal moves.

-}

--only use underlying representation below here?

evaluateMaterial :: Position ->  Float 
evaluateMaterial pos = IM.fold (\p counter -> counter + (pieceValue p)) 0  (board pos)

--hardcoded 
pieceValue :: Piece -> Float
pieceValue p@(Piece White _) = shapeValue p 
pieceValue p@(Piece Black _) = -(shapeValue p)

shapeValue :: Piece -> Float
shapeValue (Piece _ Pawn) = 1
shapeValue (Piece _ Knight) = 3
shapeValue (Piece _ Bishop) = 3
shapeValue (Piece _ Rook) = 5
shapeValue (Piece _ Queen) = 9
shapeValue _ = 0 --No point in giving a king a material rating 