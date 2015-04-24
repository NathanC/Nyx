module MoveFramework(
  prettyPrint,
  fullyLegal,
  allLegalMoves,
  toRawNum,
  fromRawNum,
  makeMove,
  constructMove,
  Move (..)
  ) where 


import BoardFramework
import qualified Data.IntMap as IM
import Data.IntMap ((!))

import Data.Maybe (mapMaybe, isNothing)
import Data.List.Split (chunksOf)
import Data.List (intercalate, intersperse)

import qualified Data.Map as M

data SpecialCase = EnPassant Square | Promotion Piece | Castle CastleType deriving Show

type Square = (Int,Int)
type Capture = Piece


data Move = Move Square Square (Maybe Capture) (Maybe SpecialCase) deriving Show
--represent a move from Square to Square, with SpecialCase present iff the move is not a brute force move that simply moves 
--the first square to the second square, replacing the second square if possible.
--a move gives enough info to map from Board -> Board. 
--the moving might have other effects, like triggering enPassant or removing castling rights,
--but the move itself doesn't keep track of this



constructMove :: Square -> Square -> Position -> Move
constructMove starting@(f1,r1) ending@(f2,r2) pos =

  case movingPiece of
    (Piece c Pawn ) 
         | (abs (f1-f2) == 1) && (isNothing maybeCapture) ->  Move starting ending (Just (Piece (switchColor c) Pawn)) (Just (EnPassant (f2,r1)))
         | r2 == 8 -> Move starting ending maybeCapture (Just (Promotion (Piece White Queen)))
         | r2 == 1 -> Move starting ending maybeCapture (Just (Promotion (Piece Black Queen)))
         | otherwise -> Move starting ending maybeCapture Nothing

    _ -> Move starting ending maybeCapture Nothing

    --_ -> Move starting ending maybeCapture Nothing

  where movingPiece = (board pos) ! (toRawNum starting)
        maybeCapture = IM.lookup (toRawNum ending) (board pos) 


--makes a move (brute forces, check for full legality first), changes color, and toggles en-pessant if appropriate 
makeMove :: Move -> Position -> Position
makeMove (Move starting ending mCap mSC) pos = pos {
      enPassant = if triggersEnPassantState starting ending
                    then Just (enPassantSquare ending)
                    else Nothing
    , toMove = switchColor currentColor
    , board = newBoard
    
    }-- add en-pessant state and changing castle rights

  where triggersEnPassantState (_,2) (_,4) = isPawn movingPiece
        triggersEnPassantState (_,7) (_,5) = isPawn movingPiece
        triggersEnPassantState   _     _   = False 

        enPassantSquare (f,4) = toRawNum (f,3)
        enPassantSquare (f,5) = toRawNum (f,6)

        movingPiece = (board pos) ! (toRawNum starting)

        currentColor = toMove pos
        b = board pos

        newBoard = case mSC of 
          Nothing -> IM.insert (toRawNum ending) movingPiece $ IM.delete (toRawNum starting) b

          Just(sc) ->
            case sc of 
              EnPassant square -> IM.insert (toRawNum ending) movingPiece $ IM.delete (toRawNum starting) $ IM.delete (toRawNum square) b

              Promotion piece -> IM.insert (toRawNum ending) piece $ IM.delete (toRawNum starting) b

              --Castle castleType -> 


-- | Returns a list of all the fully legal moves in a given position, for a certain color
allLegalMoves :: Position -> [Move]
allLegalMoves pos = filter legal $ allPseudoLegalMoves pos 

  where legal mv = if null [ m | m@(Move _ _ (Just (Piece _ King)) _) <- (allPseudoLegalMoves (makeMove mv pos))]  
                      then True
                      else False


    {-case mSC of 
    Nothing ->

    Just sc -> case sc of
      EnPassant square ->

      Promotion piece ->

      Castle CastleType ->  
-}


  


prettyPrint :: Position -> String
prettyPrint pos = intercalate "\n" $ map concat $ map (intersperse "|") $ chunksOf 8 $ map show' [(b,a) | a <- [8,7..1] , b <- [1..8] ]    
        where b = board pos
              show' x = case (IM.lookup (toRawNum x) b) of
                          Nothing -> "_"
                          Just p -> show p

--hardcode these to make them quicker?----
toRawNum :: (Int,Int) -> Int
toRawNum (a,b) = a+8*(b-1)

fromRawNum :: Int -> (Int,Int)
fromRawNum n = 
    (file, ((n-file) `div` 8)+ 1 )
    where file = if file' == 0 then 8 else file'
          file' = n `mod` 8
------------------------------------------


data Direction = U | D | L | R | UL | UR | DL | DR deriving (Eq, Ord, Show)

ray :: M.Map (Direction, Int, Int) [Square]
ray = M.fromList $ concatMap (\d -> (map (\(f,r) -> ((d,f,r), ray' d (f,r)))  [(file,rank) |  file <- [1..8], rank <- [1..8]]))  [U , D , L , R , UL , UR , DL , DR]   


ray' :: Direction -> Square -> [Square]
ray' U  (file,rank) = [ (file,rank')  | rank' <- [rank+1..8] ]    
ray' UR (file,rank) = [ (file',rank') | (file',rank') <- zip [file+1..8] [rank+1..8 ] ]
ray' UL (file,rank) = [ (file',rank') | (file',rank') <- zip [file-1,file-2..1] [rank+1..8 ] ]
ray' R  (file,rank) = [ (file',rank)  | file' <- [file+1..8] ]
ray' L  (file,rank) = [ (file',rank)  | file' <- [file-1,file-2..1] ]
ray' D  (file,rank) = [ (file,rank')  | rank' <- [rank-1,rank-2..1] ]
ray' DR (file,rank) = [ (file',rank') | (file',rank') <- zip [file+1..8] [rank-1,rank-2..1 ] ]
ray' DL (file,rank) = [ (file',rank') | (file',rank') <- zip [file-1,file-2..1] [rank-1,rank-2..1 ] ]


-- | Traces in a direction as far as it can go, returning a list of the squares that are avaible in that direction.
-- returns a list of (Square, Maybe Piece) touples that says if the square can be moved to in that
-- direction, and contains Maybe Piece if there's a capture there. So that moves that are captures can be seen as 
-- candidate moves easily.

inRange (f,r) = (f <= 8 && f >= 1) && (r <= 8 && r >= 1)


trace :: Piece -> Square -> Position -> Direction -> [Move]
trace piece starting@(f,r) pos dir = traceThrough toTrace
    where toTrace = (ray) M.! (dir,f,r)
          traceThrough [] = []
          traceThrough (x:xs) =
            case (IM.lookup (toRawNum x) (board pos)) of 
               Nothing -> (Move starting x Nothing Nothing):traceThrough xs
               Just p -> if isPawn piece && (dir == U || dir == D)
                           then [] --pawns can't capture going foward
                           else if isOpposingColor piece p
                                  then [Move starting x (Just p) Nothing]
                                  else []

traceKnight :: Piece -> Square -> Position -> [Move]
traceKnight piece starting@(f,r) pos = mapMaybe tryMoving $ filter inRange [(f-1,r+2),(f+1,r+2),(f-2,r+1),(f+2,r+1),(f-2,r-1),(f+2,r-1),(f-1,r-2),(f+1,r-2)] 
                              where tryMoving x =
                                      case IM.lookup (toRawNum x) (board pos) of
                                         Nothing -> Just (Move starting x Nothing Nothing) 
                                         Just p -> if isOpposingColor piece p
                                                      then Just (Move starting x (Just p) Nothing)
                                                      else Nothing

-- | Works, but ugly. Consider cleaning up. 
tracePawn :: Piece -> Square -> Position -> [Move]
tracePawn piece starting@(f,r) pos =
  case piece of
    (Piece White Pawn) ->
      (if(r == 2)
        then take 2 $ trace piece starting pos U
        else take 1 $ trace piece starting pos U)
      ++ mapMaybe tryMoving diagonalMoves
      ++ if (r == 5)
           then case (enPassant pos) of
             Nothing -> []
             Just x -> if (f',r') `elem` diagonalMoves     
                         then [Move starting (f',r') (Just (Piece Black Pawn)) (Just(EnPassant(f',5)))]
                         else []
                       where (f',r') = fromRawNum x
           else [] 
      where diagonalMoves = filter inRange [(f-1,r+1),(f+1,r+1)]
    (Piece Black Pawn) ->
      (if(r == 7)
        then take 2 $ trace piece starting pos D
        else take 1 $ trace piece starting pos D)
      ++ mapMaybe tryMoving diagonalMoves
      ++ if (r == 4)
           then case (enPassant pos) of
             Nothing -> []
             Just x -> if (f',r') `elem` diagonalMoves     
                         then [Move starting (f',r') (Just (Piece White Pawn)) (Just(EnPassant(f',4)))]
                         else []
                       where (f',r') = fromRawNum x
           else [] 
      where diagonalMoves = filter inRange [(f-1,r-1),(f+1,r-1)]
  where b = board pos
        tryMoving x = 
          case IM.lookup (toRawNum x) b of
            Nothing -> Nothing
            Just p -> if isOpposingColor piece p
                        then Just (Move starting x (Just p) Nothing)
                        else Nothing




-- | Returns a list of all the pseudo legal moves for all pieces of a given color
allPseudoLegalMoves :: Position -> [Move]
allPseudoLegalMoves pos = concat $ IM.elems $ IM.mapWithKey (\k p -> pseudoLegalMoves p (fromRawNum k) pos) $ IM.filter (\p -> getColor p == toMove pos) (board pos)

-- | Returns the square, maybe a piece that's captured, and maybe a piece the pawn is promoting to
pseudoLegalMoves :: Piece -> Square -> Position -> [Move]
pseudoLegalMoves piece starting pos 
    | isPawn piece = addPromotes (getColor piece) $ tracePawn piece starting pos
    | isKnight piece = traceKnight piece starting pos
    | isBishop piece = concatMap (trace piece starting pos) [UL , UR , DL , DR]
    | isRook piece = concatMap (trace piece starting pos) [U , D , L , R ]
    | isQueen piece = concatMap (trace piece starting pos) [U , D , L , R , UL , UR , DL , DR]
    | isKing piece = concatMap (\x -> take 1 (trace piece starting pos x)) [U , D , L , R , UL , UR , DL , DR]
   


  where addPromotes c moves = concat $ map (\m@(Move _ (_,r) _ _ )-> if (r == 8 || r == 1) then (promotes c m) else [m]) moves
        promotes c (Move starting ending capture _) = map (\shape -> (Move starting ending capture (Just (Promotion (Piece c shape))))) [Queen,Rook,Knight,Bishop]


-- | not as a filter for allPseudoLegalMoves
fullyLegal :: Move -> Position -> Bool 
fullyLegal mv@(Move starting ending _ _) pos =
  case maybePiece of 
    Nothing -> False --not legal move if starting square doesn't have a piece in it
    Just piece -> if (getColor piece) /= (toMove pos) 
                    then False
                    else let moves = pseudoLegalMoves piece starting pos 
                         in if ending `elem` (map (\(Move _ e _ _) -> e) moves)
                              then 
                                let nextPos = makeMove mv pos in
                                  if null [ m | m@(Move _ _ (Just (Piece _ King)) _) <- (allPseudoLegalMoves nextPos)]  
                                    then True
                                    else False
                              else False 

  where b = board pos
        maybePiece = IM.lookup (toRawNum starting) b




{-  

allmoves = map trace allpieces of color 

legalMove for bishop = traceUR + traceUL + traceDL + traceDR 

57 58 59 60 61 62 63 64
49 50 51 52 53 54 55 56
41 42 43 44 45 46 47 48
33 34 35 36 37 38 39 40
25 26 27 28 29 30 31 32
17 18 19 20 21 22 23 24
9  10 11 12 13 14 15 16
1  2  3  4  5  6  7  8  


type Square = (File,Rank) 
type Rank = Int
data File = A|B|C|D|E|F|G|H


legalMoves Pawn = moveMap (pawn position, U, takeWhile (no piece in))

--I like this idea!

leftRay 

knights?

(61,54) -> List

(30,UL) -> [39,48]
-}
