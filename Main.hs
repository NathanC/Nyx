import HTk.Toplevel.HTk hiding (Position)
import qualified HTk.Toplevel.HTk as HTk 

import Control.Monad
import System.Random
import Control.Concurrent.MVar
import Data.Map ((!))
import qualified Data.Map as M 
import Data.List (minimumBy,maximumBy)
import Data.Maybe


import qualified Data.IntMap as IM

import BoardFramework
import MoveFramework
import PositionEvaluation 
import Search

import System.Exit
import Control.Exception hiding (evaluate)

data GameInfo = GameInfo {
  ply :: Int,
  playerColor :: Color
}

data GlobalInfo = GlobalInfo {
  pos :: Position,
  photoMap :: M.Map Piece Image,
  imageMap :: M.Map (Int,Int) ImageItem,
  selected :: Maybe (Int,Int),
  cnv :: Canvas,
  gameInfo :: GameInfo
}



main:: IO ()
main = do 
  putStrLn "Welcome to the Nyx Chess Engine!"
  putStrLn "Please enter the search depth: "
  ply' <- getLine
  gui $ GameInfo (read ply') White

gui info = do 
    darkWood <- newImage [filename "resources/darkWood.png"]
    lightWood <- newImage [filename "resources/whiteWood.png"]
    border <- newImage [filename "resources/border.png"]

    w <- initHTk [maxSize (500,500), minSize (500,500), text "Nyx"]    
    cnv' <- newCanvas w [width 500, height 500]

    -- generate board and border
    mapM (\((x,y),i)-> createImageItem cnv' [position (50*x,50*y), canvAnchor NorthWest, photo i]) $ zip [(x,y) | x <- [1..8], y<-[1..8]] $ cycle (take 8 (cycle [lightWood,darkWood]) ++ take 8 (cycle [darkWood,lightWood]))
    mapM (\(x,y)-> createImageItem cnv' [position (50*x,50*y), canvAnchor NorthWest, photo border]) $ zip [0..9] (cycle [0]) ++ zip [0..9] (cycle [9]) ++  zip (cycle [0]) [1..8] ++ zip (cycle [9]) [1..8]

    
    --create events for clicking, releasing, and moving the mouse 
    (click, _) <- bind cnv' [WishEvent [] (ButtonPress (Just 1))]
    (release,_) <- bind cnv' [WishEvent [] (ButtonRelease (Just 1))]
    (motion, _ ) <- bind cnv' [WishEvent [Button1] Motion]

    let pos' = newPosition
    
    photoMap' <- genPhotoMap
    imageMap' <- renderPosition pos' cnv' photoMap'

    globalInfoMVar <- newMVar $ GlobalInfo pos' photoMap' imageMap' Nothing cnv' info

    spawnEvent $ forever $ do

      (click >>>= clickHandler globalInfoMVar)

      +>

      (release >>>= releaseHandler globalInfoMVar) 

      +>

      (motion >>>= motionHandler globalInfoMVar) 


    pack cnv' []
    finishHTk


    




clickHandler globalInfoMVar i = do
  globalInfo <- takeMVar globalInfoMVar

  case nearestSquare (x i, y i) of
    Nothing -> putMVar globalInfoMVar globalInfo
    Just starting ->
      if (toRawNum starting) `elem` (IM.keys (board (pos globalInfo)))
               then putMVar globalInfoMVar globalInfo{selected = Just starting}
               else putMVar globalInfoMVar globalInfo
  

releaseHandler globalInfoMVar i = do
  globalInfo <- takeMVar globalInfoMVar

  case (selected globalInfo) of
    Nothing -> putMVar globalInfoMVar globalInfo
    Just starting -> 
      case (nearestSquare (x i,y i)) of
        Nothing -> do 
          (imageMap globalInfo ! starting)  # position (toCanvasSquare starting)
          putMVar globalInfoMVar globalInfo{selected = Nothing}
        Just ending -> do
          if fullyLegal (constructMove starting ending (pos globalInfo)) (pos globalInfo)
            then do
              (imageMap globalInfo ! starting) # position (toCanvasSquare ending)
              let nextPosition = (makeMove (constructMove starting ending (pos globalInfo)) (pos globalInfo))
              mapM_ destroy $ M.elems $ imageMap globalInfo
              newImageMap <- renderPosition nextPosition (cnv globalInfo) (photoMap globalInfo)
              putMVar globalInfoMVar globalInfo{pos = nextPosition, imageMap = newImageMap, selected = Nothing}              
              
              calculateAndMakeMove globalInfoMVar  
                         
           else do
             (imageMap globalInfo ! starting)  # position (toCanvasSquare starting)
             putMVar globalInfoMVar globalInfo{selected = Nothing}


calculateAndMakeMove globalInfoMVar = do
  globalInfo <- takeMVar globalInfoMVar
  forFun <- randomFrom ponderingStatements
  putStrLn forFun

  let (bestMove@(Move starting ending _ _), _) = calculateMove (pos globalInfo ) $ ply $ gameInfo globalInfo

  nextPosition <- return $! makeMove bestMove $ pos globalInfo
  mapM_ destroy $ M.elems $ imageMap globalInfo
  newImageMap <- renderPosition nextPosition (cnv globalInfo) (photoMap globalInfo)
  putMVar globalInfoMVar globalInfo{pos = nextPosition, imageMap = newImageMap}
  putStrLn $ "I'll move from " ++ printNice starting ++ " to " ++ printNice ending ++ ".\n"          
  
  where printNice (f,r) = fileCharMap ! f : show r            

randomFrom xs = do 
  index <- getStdRandom (randomR (0,(length xs)-1))
  return $ xs !! index


motionHandler globalInfoMVar i = do
  globalInfo <- takeMVar globalInfoMVar 

  case selected globalInfo of 
    Nothing -> putMVar globalInfoMVar globalInfo
    Just starting -> do 
      ((imageMap globalInfo) ! starting) # putItemOnTop
      ((imageMap globalInfo) ! starting) # position (x i, y i)
      putMVar globalInfoMVar globalInfo





genPhotoMap :: IO (M.Map Piece Image)
genPhotoMap = do
  whitePawn <- newImage [filename "resources/whitePawn.png"]
  whiteQueen <- newImage [filename "resources/whiteQueen.png"]
  whiteRook <- newImage [filename "resources/whiteRook.png"]
  whiteKnight <- newImage [filename "resources/whiteKnight.png"]
  whiteBishop <- newImage [filename "resources/whiteBishop.png"]
  whiteKing <- newImage [filename "resources/whiteKing.png"]

  blackPawn <- newImage [filename "resources/blackPawn.png"]
  blackQueen <- newImage [filename "resources/blackQueen.png"]
  blackRook <- newImage [filename "resources/blackRook.png"]
  blackKnight <- newImage [filename "resources/blackKnight.png"]
  blackBishop <- newImage [filename "resources/blackBishop.png"]
  blackKing <- newImage [filename "resources/blackKing.png"]

  return $ M.fromList $ [(Piece White Pawn, whitePawn), 
                         (Piece White Queen, whiteQueen),
                         (Piece White Rook, whiteRook),
                         (Piece White Knight, whiteKnight),
                         (Piece White Bishop, whiteBishop),
                         (Piece White King, whiteKing),
                         (Piece Black Pawn, blackPawn),
                         (Piece Black Queen, blackQueen),
                         (Piece Black Rook, blackRook),
                         (Piece Black Knight, blackKnight),
                         (Piece Black Bishop, blackBishop),
                         (Piece Black King, blackKing)]


-- renders the position onto the canvas, and returns a map of position -> Image item
renderPosition :: Position -> Canvas -> M.Map Piece Image -> IO (M.Map (Int,Int) ImageItem)
renderPosition pos cnv photos = do

    imageMap <- mapM (\(sq,p) -> do
                                 i <- createImageItem cnv [position (toCanvasSquare sq), photo (photos ! p ) ]
                                 return (sq,i ) ) board'
    return $ M.fromList imageMap

  where board' = map (\(k,p) -> (fromRawNum k, p ) ) $ IM.toList $ board pos   



toCanvasSquare :: (Int,Int) -> (Distance,Distance)
toCanvasSquare (f,r) = (Distance (25+50*f), Distance (25+50*(9-r)))


nearestSquare :: (Distance,Distance) -> Maybe (Int,Int)
nearestSquare pos = 
  case nearestCanvasSquare pos of 
    Nothing -> Nothing
    Just (x,y) -> Just (fileMap ! x, rankMap ! y)


nearestCanvasSquare :: (Distance,Distance) -> Maybe (Distance,Distance)
nearestCanvasSquare (x,y) = 
  case (nearest x, nearest y) of
    (Just x', Just y') -> Just (x',y')
    _                  -> Nothing


nearest :: Distance -> Maybe Distance 
nearest x = if distance > 25 
              then Nothing
              else Just val

    where (val,distance) = minimumBy (\(_,b1) (_,b2) -> compare b1 b2) [ (n, abs (n-x)) | n <- [75,125..425]  ]  





-- Various maps used for translating coordinate systems
rankMap :: M.Map Distance Int
rankMap = M.fromList $ zip [75,125..425] [8,7..1]

fileMap :: M.Map Distance Int
fileMap = M.fromList $ zip [75,125..425] [1..8]

fileCharMap :: M.Map Int Char
fileCharMap = M.fromList $ zip [1..8] ['a'..'h']


ponderingStatements = [
  "Hmm, interesting..",
  "Ha! You sure that's a good move?",
  "Well, well, well..",
  "You play like a drunken dwarf!",
  "Good move!",
  "Meh..",
  "Let me think about that for a bit...",
  "That's a tough one.",
  "That was a slip of the hand... right?",
  "Nice try, but..",
  "Clever.",
  "I think you missed something..."]