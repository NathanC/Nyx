module PositionEvaluation (evaluate) where

import BoardFramework
import MoveFramework
import qualified Data.IntMap as IM 
import qualified Data.Map as M

import Data.List

--to use the Infinity val as a float in a cleaner way
infinity :: Float
infinity = (1/0)

allLegalMoves' :: Position -> [Move] -> [Move]
allLegalMoves' pos pl = filter legal pl

  where legal mv = if null [ m | m@(Move _ _ (Just (Piece _ King)) _) <- (allPseudoLegalMoves (makeMove mv pos))]  
                      then True
                      else False

-- I CAN make a move that's infinitly good. I CANNOT make a move that is infinitly bad..? Sometimes I have to...
-- Can't make a move that is infinitly bad in 1 depth? Can in more than 1?

-- ooh, I check for checkmate and stalemate EXTERNALLY from alpha-beta, after each move by me and machine! So if it makes a move where I have no
-- legal moves, I'm in checkmate. If I make a move where it has no legal moves, it's in checkmate. So it tries to make a move.. then I check if 
-- it's legal.

-- takes a position, and calculates the rating
-- MAYBE evalate to checkmate or stalemate as well? Would make easier.. would work well for leaves anyhow, once
-- I defer checking for full legality.
evaluate :: Position -> Float
evaluate pos 
    | checkmated = infinitelyBadForMover 
    | stalemated = 0 
    | otherwise = 
        evaluateMaterial pos + --bishop pair, knight and rook closed/open bias, and static material
        evaluateSpace pseudoLegalWhite pseudoLegalBlack + -- favors open positions
        favorCastling pos  --favors castling 

        --implement DEEP TRACE with all the squares CONTROLLED, with pseudolegalmoves as a subset of the squares controlled
        --can be generated from a deep trace pretty easily. Will the deep trace be too inefficent though?
        -- problably not. Knights are the same (except notes if it's attacking their own color), pawns don't add much (is a deep trace 
        -- forward worth it?), bishops and queens and rooks will add more, but not by a ton. 

        --with deep trace, I can favor central control (more heavily weighted if not xraying? xraying decreasing power?)
        
    where pseudoLegalBlack = allPseudoLegalMoves $ pos{toMove = Black}
          pseudoLegalWhite = allPseudoLegalMoves $ pos{toMove = White}
          allLegalBlack= allLegalMoves' (pos{toMove = Black}) pseudoLegalBlack
          allLegalWhite = allLegalMoves' (pos{toMove = White}) pseudoLegalWhite

          blackCanTakeKing = not $ null [ m | m@(Move _ _ (Just (Piece _ King)) _) <- pseudoLegalBlack]  
          whiteCanTakeKing = not $ null [ m | m@(Move _ _ (Just (Piece _ King)) _) <- pseudoLegalWhite]

          checkmated = case (toMove pos) of
            White -> (null allLegalWhite) && blackCanTakeKing
            Black -> (null allLegalBlack) && whiteCanTakeKing

          stalemated = case (toMove pos) of
            White -> (null allLegalWhite)
            Black -> (null allLegalBlack)

          infinitelyBadForMover = case (toMove pos) of
            White -> -infinity
            Black -> infinity

          --infinitelyGoodForMover = 

{-
material, attacked pieces, doubled pawns, knight on rim, bishop pair. Kings being captured at -inf? will prevent having to check if can be captured 
when calculating pseudo-legal moves.


how ought I best to find these out?


prefer open files for rooks
prefer outposts (specifically forward) for knights
prefer central control
prefer targets to no targets
prefer overloading ones own pieces
prefer 7th rank for rook
prefer knights not on rim
prefer pawns protected by pawns (prefer pawn chains)
prefer space
prefer doubled rooks (gonna have to add "hits" for pieces of same type, perhaps even rays through them for xray attacks)
prefer pins
prefer advanced pawns
highly prefer long diagnals for bishops (and queens), building on space advantage

!!!!!prefer attacking around enemy king (find where enemy king is, prefer moves that influence that area of the board)!!!
        -this could be very useful in the position trying to go towards an overarching aim


prefer non-repition
prefer not losing tempo

prefer developed over non-developed (mainly an opening thing)

!!!!!prefer trading material when up!!!!!!... material value some sort of percentage?

experimental: prefer sacrificting (prefer positions that minimize overall materrial?)
    -or vice versa

currently wastes far too many tempos

and repeates!! need to implement 3 move repitition. For now, as game history, with FEN positions? 
    -export to PNG after game?

like this idea: "Poisoned pieced". When I make a move, I check if the piece is in a list of pieces recently moved, if so, it has a "poison"
    level in it, that builds up, and is counted in the eval. function
    Only needs to exist for, say, the previous 10 moves, but will be good in making the computer
    hesitant about moving pieces multiple times.. maybe less for pawns than other pieces though? And maybe more in early game than late game?
    Either way, there should be a maximum level of poisioning, that it caps out at.
    !!!!!Oh, positions that repeat once (so, 2 times rep. for pos.) should have a poision level too?

-}

--only use underlying representation below here?

evaluateMaterial :: Position ->  Float 
evaluateMaterial pos = (value White) - (value Black)

  where counterMap = IM.fold (\p m -> M.insertWith' (+) p 1 m ) M.empty (board pos)
        getCount color shape = M.findWithDefault 0 (Piece color shape) counterMap
        value c = 
            let pawns = (getCount c Pawn)
                pawnsAboveFive = pawns - 5
            in 0 + 
               1 * pawns +
               3.25 * (getCount c Bishop) +
               ((3.25 + pawnsAboveFive * (1/8)) * (getCount c Knight)) + --fix for each knightq
               ((5 + pawnsAboveFive * (-1/16)) * (getCount c Rook) ) +
               9.75 * (getCount c Queen) +
               if (getCount c Bishop) == 2 
                 then 0.5
                 else 0 
               
-- prefer open positions
evaluateSpace :: [Move] -> [Move] -> Float
evaluateSpace w b = fromIntegral (length w) * (1/5) - fromIntegral (length b) * (1/5)
--experimental. VERY HIGH value on space if a pawn sacrafice gains more than 5 spaces, this considers it worth it

-- Having castled is worth a pawn and a 3/4th. Losing castle rights is worth half a pawn for each castle right lost.
-- So castling has a net gain of 3/4th a pawn.
favorCastling pos = (wq + wk + bq + bk) + (wq' + wk' + bq' + bk')

    where history = hasCastled pos
          rights = (castleRights pos)

          wq = if WhiteQueenside `elem` history
                 then 2
                 else 0
          wk = if WhiteKingside `elem` history
                 then 2 
                 else 0
          bq = if BlackQueenside `elem` history 
                 then -2
                 else 0
          bk = if BlackKingside `elem` history
                 then -2
                 else 0

          wq' = if WhiteQueenside `elem` rights
                 then (1/2)
                 else 0
          wk' = if WhiteKingside `elem` rights
                 then (1/2) 
                 else 0
          bq' = if BlackQueenside `elem` rights 
                 then -(1/2) 
                 else 0
          bk' = if BlackKingside `elem` rights
                 then -(1/2)
                 else 0
