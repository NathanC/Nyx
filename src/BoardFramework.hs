module BoardFramework( 
Position (..), 
Piece (..), 
Color(..),
Shape (..),
CastleType(..), 
isOpposingColor, 
getColor,
newPosition,
isBlack,
isWhite,
isPawn,
isKnight,
isBishop,
isRook,
isQueen,
isKing,
switchColor) where

--limit the scope of this, to simply contain board (+position?) state and ways of accessing/changing state


import Data.Maybe
import Data.IntMap

--board rep. IntMap from [1..64] -> Piece
data Position = Position {
                   board :: IntMap Piece
                 , enPassant :: Maybe Int
                 , castleRights :: [CastleType]
                 , toMove :: Color
                 , hasCastled :: [CastleType]
                }
                deriving Show


data CastleType = WhiteKingside | WhiteQueenside | BlackKingside | BlackQueenside deriving (Show, Eq)
data Piece = Piece Color Shape deriving (Eq, Ord)
data Color = White | Black deriving (Show, Eq, Ord, Enum, Bounded)
data Shape = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Ord, Enum, Bounded)

instance Show Piece where 
   show (Piece White Pawn)   = "P"
   show (Piece White Rook)   = "R"
   show (Piece White Knight) = "N"
   show (Piece White Bishop) = "B"
   show (Piece White Queen)  = "Q"
   show (Piece White King)   = "K"
   show (Piece Black Pawn)   = "p"
   show (Piece Black Rook)   = "r"
   show (Piece Black Knight) = "n"
   show (Piece Black Bishop) = "b"
   show (Piece Black Queen)  = "q"
   show (Piece Black King)   = "k"
    
   
getColor (Piece White _) = White
getColor (Piece Black _) = Black

 
switchColor White = Black
switchColor Black = White


isWhite :: Piece -> Bool
isWhite (Piece White _) = True
isWhite _ = False

isBlack :: Piece -> Bool
isBlack (Piece Black _) = True
isBlack _ = False

isOpposingColor :: Piece -> Piece -> Bool
isOpposingColor (Piece White _) (Piece Black _) = True
isOpposingColor (Piece Black _) (Piece White _ ) = True
isOpposingColor _ _ = False 


isPawn  :: Piece -> Bool
isPawn (Piece _ Pawn) = True
isPawn _ = False

isKnight  :: Piece -> Bool
isKnight (Piece _ Knight) = True
isKnight _ = False

isBishop  :: Piece -> Bool
isBishop (Piece _ Bishop) = True
isBishop _ = False

isRook  :: Piece -> Bool
isRook (Piece _ Rook) = True
isRook _ = False

isQueen  :: Piece -> Bool
isQueen (Piece _ Queen) = True
isQueen _ = False

isKing  :: Piece -> Bool
isKing (Piece _ King) = True
isKing _ = False



newPosition :: Position
newPosition = Position newBoard Nothing [WhiteKingside, WhiteQueenside, BlackKingside, BlackQueenside] White []

newBoard :: IntMap Piece
newBoard = fromList $ [(x,Piece White c) | (x,c) <- zip [1..8] [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]] ++
                      [(x,Piece White Pawn) | x <- [9..16]] ++
                      [(x,Piece Black Pawn) | x <- [49..56]] ++
                      [(x,Piece Black c) | (x,c) <- zip [57..64] [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]]





{-
--attempts the move, returns Just(Board) if move is legal, returns Nothing if not.
--move islegal IFF move conforms to movement constraints of piece, and king is not in check after move
--this should be an exposed function
move :: Position -> Move -> Maybe Position

--generates a new board, with standard set-up

fromFEN :: FEN -> Position

fromPNG?

-}



