module Search (
    calculateMove,
    EndCondition(..)
    ) where

import BoardFramework
import MoveFramework
import PositionEvaluation

import Data.List(minimumBy,sortBy)
import Control.Parallel.Strategies
import Control.DeepSeq

-- This module is used to determine the best move in a given position

-- To use the Infinity val as a float in a cleaner way
infinity :: Float
infinity = (1/0)

data EndCondition = Checkmate | Draw 

-- Needed for the "rdeepseq" parMap strategy
instance NFData Move where 
   rnf _ = ()

-- This function searches for the best move it can find, and returns it (along with a potential endcondition flag)
-- The different legal moves are evaluated in parallel 
calculateMove :: Position -> Int -> (Move, Maybe EndCondition)
calculateMove pos depth = (fst $ minimumBy (\(_,a) (_,b) -> compare a b) moveEvals, Nothing)

    where  evaluateMove m = (m, alphabeta (makeMove m pos) depth (-infinity) infinity True) 
           candidateMoves = allLegalMoves pos
           moveEvals = parMap rdeepseq evaluateMove candidateMoves

-- need to implement null childPositions case and checkmate stuff 
alphabeta :: Position -> Int -> Float -> Float -> Bool -> Float
alphabeta pos depth alpha beta maximizingPlayer 
    | depth == 0 = evaluate pos
    | maximizingPlayer = maximizingPlayerFunc (-infinity) depth alpha beta childPositions
    | not maximizingPlayer = minimizingPlayerFunc infinity depth alpha beta childPositions

  where childPositions = map (\m -> makeMove m pos) $ sortBy (\(Move _ _ c _) (Move _ _ c2 _) -> compare c2 c) $ allLegalMoves pos 

        maximizingPlayerFunc v depth _ _ [] = v
        maximizingPlayerFunc v depth alpha beta (x:xs) = 
            let v' = max v (alphabeta x (depth-1) alpha beta False)
                alpha' = max alpha v'
            in if beta <= alpha' 
                  then v' 
                  else maximizingPlayerFunc v' depth alpha' beta xs

        minimizingPlayerFunc v depth _ _ [] = v
        minimizingPlayerFunc v depth alpha beta (x:xs) =
            let v' = min v (alphabeta x (depth-1) alpha beta True)
                beta' = min beta v'
            in if beta' <= alpha 
                 then v' 
                 else minimizingPlayerFunc v' depth alpha beta' xs  

minimax :: Position -> Int -> Bool -> Float
minimax pos depth maximizingPlayer 
    | depth == 0 = evaluate pos
    | null childPositions = 
        if (toMove pos) ==  White
          then (-infinity)
          else infinity
    | maximizingPlayer =
        let evals = map (\p -> minimax p (depth-1) False ) childPositions
        in maximum $ evals ++ [-infinity]
    | not maximizingPlayer = 
        let evals = map (\p -> minimax p (depth-1) True ) childPositions
        in minimum $ evals ++ [infinity]

  where childPositions = map (\m -> makeMove m pos) $ allLegalMoves pos

