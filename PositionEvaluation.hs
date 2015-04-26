module PositionEvaluation (evaluate) where

import BoardFramework
import MoveFramework
import qualified Data.IntMap as IM 
import qualified Data.Map as M



--takes a position, and calculates the rating
--MAYBE evalate to checkmate or stalemate as well? Would make easier.. would work well for leaves anyhow, once
-- I defer checking for full legality.
evaluate :: Position -> Float
evaluate pos = evaluateMaterial pos + evaluateSpace pseudoLegalWhite pseudoLegalBlack 
    where pseudoLegalBlack= allPseudoLegalMoves $ pos{toMove = Black}
          pseudoLegalWhite = allPseudoLegalMoves $ pos{toMove = White}

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
               (3.25 * (getCount c Knight) + pawnsAboveFive * (1/8) ) +
               (5 * (getCount c Rook) + pawnsAboveFive * (-1/16)) +
               9.75 * (getCount c Queen) +
               if (getCount c Bishop) == 2 
                 then 0.5
                 else 0 
               

-- prefer open positions
evaluateSpace :: [Move] -> [Move] -> Float
evaluateSpace w b = fromIntegral (length w) * (1/5) - fromIntegral (length b) * (1/5)
--experimental. VERY HIGH value on space if a pawn sacrafice gains more than 5 spaces, this considers it worth it