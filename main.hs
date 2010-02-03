module Main () where

import Data.Array
import Data.List
import Graphics.UI.SDL as SDL

cellsize  = 40
border    = 1
padding   = 30
wwidth    = 450
wheight   = 450
cellcolor = SDL.Pixel 0x00999999
hlcolor   = SDL.Pixel 0x00991111
actcolor  = SDL.Pixel 0x00118505


level1 = Level (3,4) 31 (mapToArray map2)
map2 = ["#####    ",
        "#   #    ",
        "### ###  ",
        "#######  ",
        "#######  ",
        "#        ",
        "#########"]

data Cell = Full | Empty
        deriving (Eq, Show)

data Level = Level {
        lStart  :: (Int,Int),
        lLength :: Int,
        lMap    :: Array (Int,Int) Cell
    } deriving (Show)

mapToArray map = listArray ((1,1),(w,h)) [conv x | x <- concat.transpose $ map]
    where h = length map
          w = length $ head map
          conv ch = case ch of
              '#' -> Full
              _   -> Empty

main = do SDL.init [SDL.InitVideo]
          setVideoMode wwidth wheight 32 [HWSurface, DoubleBuf]
          setCaption "hKerixep" []
          start (lMap level1)

start board = do
          screen<- SDL.getVideoSurface
          mapM_ (draw screen cellcolor)
                [(x,y)|((x,y),Full) <- assocs (lMap level1)]
          draw screen actcolor (lStart level1)
          SDL.flip screen
          eventHandler [lStart level1]

draw screen color (x,y) = SDL.fillRect screen (Just rect) color
        where rect = SDL.Rect (pos x) (pos y) fill fill
              pos n = padding + (n - 1) * cellsize
              fill = cellsize - border

legalmove (x, y) clist@(c:cs) =
                   if elem (x,y)  (indices (lMap level1))
                      && (lMap level1 ! (x,y) == Full)
                      && notElem (x,y) clist
                      && neighbour (x,y) c
                   then (True, (x,y):clist)
                   else (False, c:cs)
                where neighbour (x,y) (x',y') = elem (x,y) [(x'+1,y')
                                                           ,(x'-1,y')
                                                           ,(x',y'+1)
                                                           ,(x',y'-1)]

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
    MouseMotion x y _ _ -> do let pos = position (fI x) (fI y)
                              let (en,newclist) = legalmove pos clist
                              if en then do screen<- SDL.getVideoSurface
                                            draw screen hlcolor $ head clist
                                            draw screen actcolor pos
                                            SDL.flip screen
                                            eventHandler newclist
                                    else eventHandler clist
                              where fI = fromIntegral
    MouseButtonDown _ _ _  -> start (lMap level1)
    _ -> eventHandler clist
