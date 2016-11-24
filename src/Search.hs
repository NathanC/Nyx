module Search (
    calculateMove,
    EndCondition(..)
    ) where

import BoardFramework
import MoveFramework
import PositionEvaluation

import Data.List(minimumBy,sortBy)

-- This module is used to determine the best move in a given position

--to use the Infinity val as a float in a cleaner way
infinity :: Float
infinity = (1/0)

data EndCondition = Checkmate | Draw 

calculateMove :: Position -> Int -> (Move, Maybe EndCondition)
calculateMove pos depth = (fst $ minimumBy (\(_,a) (_,b) -> compare a b) moveEvals, Nothing)

    where  candidateMoves = allLegalMoves pos
           moveEvals = [(m, alphabeta (makeMove m pos) depth (-infinity) infinity True) | m <- candidateMoves ]

{-
calculateMove :: Position -> Int -> (Move, Maybe EndCondition)
calculateMove pos depth = (fst $ minimumBy (\(_,a) (_,b) -> compare a b) moveEvals, Nothing)

    where  candidateMoves = allLegalMoves pos
           moveEvals = [(m, minimax (makeMove m pos) depth True) | m <- candidateMoves ]
-}

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

--minimax2 :: Position -> Int -> Bool -> Float 
--minimax2 pos depth maximizingPlayer 
--   | depth == 0 = evaluatePos 
