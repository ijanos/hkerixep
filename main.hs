module Main () where

import Data.Array
import Graphics.UI.SDL as SDL

cellsize  = 40
border    = 1
padding   = 30
wwidth    = 450
wheight   = 450
cellcolor = SDL.Pixel 0x00999999
hlcolor   = SDL.Pixel 0x00991111
actcolor  = SDL.Pixel 0x00118505

map1 = [[Full,Full,Full,Full,Full,Full],
        [Empty,Full,Full,Full,Full,Full],
        [Full,Full,Empty,Empty,Full,Full],
        [Full,Full,Full,Full,Full,Full],
        [Full,Full,Full,Full,Full,Full],
        [Full,Full,Empty,Empty,Full,Full]
       ]

mapToArray map = listArray ((1,1),(h,w)) $ concat map
    where h = length map
          w = length $ head map

data Cell = Full | Empty
        deriving (Eq, Show)

board = mapToArray map1

main = do SDL.init [SDL.InitVideo]
          setVideoMode wwidth wheight 32 [HWSurface, DoubleBuf]
          setCaption "hKerixep" []
          start board

start board = do
          screen<- SDL.getVideoSurface
          mapM_ (draw screen cellcolor) [(x,y)|((x,y),Full) <- assocs board]
          draw screen actcolor (1,1)
          SDL.flip screen
          eventHandler [(1,1)]

draw screen color (x,y) = SDL.fillRect screen (Just $ SDL.Rect (pos x) (pos y) fill fill) color
        where pos n = padding + (n - 1) * cellsize
              fill = cellsize - border

legalmove (x, y) clist@(c:cs) =
                   if elem (x,y)  (indices board)
                      && (board ! (x,y) == Full)
                      && notElem (x,y) clist
                      && neighbour (x,y) c
                   then (True, (x,y):clist)
                   else (False, c:cs)
                where neighbour (x,y) (x',y') = elem (x,y) [(x'+1,y'),(x'-1,y'),(x',y'+1),(x',y'-1)]

position :: Int -> Int -> (Int,Int)
position curX curY = (x+1,y+1)
     where x = (curX-padding) `div` cellsize
           y = (curY-padding) `div` cellsize

eventHandler :: [(Int, Int)] -> IO ()
eventHandler clist = do
  e <- waitEvent
  case e of
    Quit -> return ()
    KeyDown (Keysym key _ _)   ->
        case key of
            SDLK_q ->  return ()
            _      ->  eventHandler clist
    MouseMotion x y _ _ -> do let pos = position (fromIntegral x) (fromIntegral y)
                              let (en,newclist) = legalmove pos clist
                              if en then do screen<- SDL.getVideoSurface
                                            draw screen hlcolor $ head clist
                                            draw screen actcolor pos
                                            SDL.flip screen
                                            eventHandler newclist
                                    else eventHandler clist
    MouseButtonDown _ _ _  -> start board
    _ -> eventHandler clist
