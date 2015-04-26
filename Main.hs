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
import Text.Read
import System.Console.ANSI (clearScreen)


data GlobalInfo = GlobalInfo {
  pos :: Position,
  photoMap :: M.Map Piece Image,
  imageMap :: M.Map (Int,Int) ImageItem,
  selected :: Maybe (Int,Int),
  cnv :: Canvas 
}

main:: IO ()
main = do 
    darkWood <- newImage [filename "resources/darkWood.png"]
    lightWood <- newImage [filename "resources/whiteWood.png"]
    border <- newImage [filename "resources/border.png"]

    w <- initHTk [maxSize (500,500), minSize (500,500), text "Nyx"]    
    cnv' <- newCanvas w [width 500, height 500]





    --whiteSquare <- newBitMap [filename "black.bmp"]
    --blackSquare <- newBitMap [filename "black.xbm"]

    --mapM  (\(x,y) -> createRectangle c [position (50*x,50*y), width 50, height 50])  [(x,y) | x <- [0..7], y<-[0..7]]


    --(createBitMapItem c [position (0,0), bitmap whiteSquare])
    

    -- generate board and border
    mapM (\((x,y),i)-> createImageItem cnv' [position (50*x,50*y), canvAnchor NorthWest, photo i]) $ zip [(x,y) | x <- [1..8], y<-[1..8]] $ cycle (take 8 (cycle [lightWood,darkWood]) ++ take 8 (cycle [darkWood,lightWood]))
    mapM (\(x,y)-> createImageItem cnv' [position (50*x,50*y), canvAnchor NorthWest, photo border]) $ zip [0..9] (cycle [0]) ++ zip [0..9] (cycle [9]) ++  zip (cycle [0]) [1..8] ++ zip (cycle [9]) [1..8]

    
    --create events for clicking, releasing, and moving the mouse 
    (click, _) <- bind cnv' [WishEvent [] (ButtonPress (Just 1))]
    (release,_) <- bind cnv' [WishEvent [] (ButtonRelease (Just 1))]
    (motion, _ ) <- bind cnv' [WishEvent [Button1] Motion]
    --Button1

    let pos' = newPosition
    
    photoMap' <- genPhotoMap
    imageMap' <- renderPosition pos' cnv' photoMap'

    globalInfoMVar <- newMVar $ GlobalInfo pos' photoMap' imageMap' Nothing cnv'

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
              putStrLn "calculating response.."
              calculateAndMakeMove globalInfoMVar  
              putStrLn "made move."            
           else do
             (imageMap globalInfo ! starting)  # position (toCanvasSquare starting)
             putMVar globalInfoMVar globalInfo{selected = Nothing}


calculateAndMakeMove globalInfoMVar = do
  globalInfo <- takeMVar globalInfoMVar

  let (bestMove, _) = calculateMove (pos globalInfo ) 4

  nextPosition <- return $! makeMove bestMove $ pos globalInfo
  mapM_ destroy $ M.elems $ imageMap globalInfo
  newImageMap <- renderPosition nextPosition (cnv globalInfo) (photoMap globalInfo)
  putMVar globalInfoMVar globalInfo{pos = nextPosition, imageMap = newImageMap}              
                



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



movePiece m p e = do
  withMVar m $ \b -> case b of
                      True -> do p # position (x e, y e)
                                 return ()
                      False -> return ()

squareStr Nothing = "the border"
squareStr (Just (x,y)) = f : r
    where f =  fileCharMap ! (fileMap ! x) 
          r =  show $ rankMap ! y


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


rankMap :: M.Map Distance Int
rankMap = M.fromList $ zip [75,125..425] [8,7..1]

fileMap :: M.Map Distance Int
fileMap = M.fromList $ zip [75,125..425] [1..8]

fileCharMap :: M.Map Int Char
fileCharMap = M.fromList $ zip [1..8] ['a'..'h']




--main = do
--    mainLoop pos "Welcome to Nyx Chess Engine!"

{-
mainLoop :: Position -> String -> IO ()
mainLoop pos buffer = do
    clearScreen
    putStrLn $ prettyPrint pos
    putStr "\n"
    putStrLn $ "message: " ++ buffer 
    putStrLn $ "Current color to move: " ++ show (toMove pos)

    putStr "Enter next move: "
    input <- getLine 

    if input == "quit"
       then exitWith ExitSuccess
       else return ()


    let (starting, ending) = parseMove input

    if (fullyLegal starting ending pos)
        then mainLoop ((move starting ending pos){toMove = succThrough (toMove pos) }) $ "Last move was " ++ input
        else mainLoop pos "Illegal move or incorrect move formatting."


-- | Simple move parse function. Needs to be of form "a1-b4". Not flexible, but simple. Returns values of -1 in the moves if something goes wrong
parseMove :: String -> ((Int,Int),(Int,Int))
parseMove str
    | length str < 5 = ((-1,-1),(-1,-1))
    | otherwise = (parseSquare starting, parseSquare ending)
  where starting = ((str !! 0), (str !! 1))
        ending = ((str !! 3), (str !! 4))
        parseSquare (a,b) = (resolve a, parse [b])
        parse a = case (readMaybe a) of
                    Nothing -> -1
                    Just v ->  v
        resolve :: Char -> Int 
        resolve 'a' = 1 
        resolve 'b' = 2
        resolve 'c' = 3
        resolve 'd' = 4
        resolve 'e' = 5
        resolve 'f' = 6
        resolve 'g' = 7
        resolve 'h' = 8
        resolve  _ = -1
-}


{-
-- nieve mini-max algorithm
bestMove :: Position -> Depth -> Color -> ((Int,Int), Maybe Piece)
bestMove _ 0 _ = 
bestMove pos x computerColor=  

    if (toMove pos) == computerColor 
        then 

        else

    bestMove $ legalMoves pos 


    --generate pos for each legal move for color

    -}

--makeMove :: (Square, Square, Maybe Piece, Maybe Piece) -> Position
--makeMove :: 