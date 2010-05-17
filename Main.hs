module Main () where

import Data.Array
import Data.List
import Graphics.UI.SDL as SDL
import Control.Monad

import Levels

cellsize   = 40
border     = 1
padding    = 30
wwidth     = 450
wheight    = 450
cellcolor  = SDL.Pixel 0x00999999
hlcolor    = SDL.Pixel 0x00991111
actcolor   = SDL.Pixel 0x0000A80E
bgcolor    = SDL.Pixel 0x00111111
startcolor = SDL.Pixel 0x00006B09

data GameState = GameState {
            levels :: [Level],
            cList  :: [(Int,Int)]
     } deriving (Show)

main ::  IO ()
main = do SDL.init []
          setVideoMode wwidth wheight 32 [HWSurface, DoubleBuf]
          setCaption "hKerixep" []
          intro  <- getLevels "intro.txt"
          levels <- getLevels "levels.txt"
          showIntro intro
          start levels
          putStrLn "a winner is you"


getCleanScreen ::  IO Surface
getCleanScreen = do screen <- SDL.getVideoSurface
                    SDL.fillRect screen
                              (Just (SDL.Rect 0 0 wwidth wheight)) bgcolor
                    return screen
                 
drawLevel ::  Surface -> Level -> IO ()
drawLevel screen = drawLevel' screen cellsize

drawLevel' ::  Surface -> Int -> Level -> IO ()
drawLevel' screen size level = do 
            mapM_ (draw' screen cellcolor size)
                  [(x,y)|((x,y),Full) <- assocs . lMap $ level]
            draw' screen startcolor size $ lStart level
            SDL.flip screen


showIntro :: [Level] -> IO ()
showIntro [logo,start] =
                    do screen <- getCleanScreen
                       drawLevel' screen 10 logo
                       drawLevel screen start
                       eventHandler $ GameState [start] []
            
                
start :: [Level] -> IO ()
start levels = do screen <- getCleanScreen
                  drawLevel screen $ head levels
                  eventHandler $ GameState levels []

draw screen color (x,y) = draw' screen color cellsize (x,y)
draw' screen color size (x,y) = SDL.fillRect screen (Just rect) color
        where rect = SDL.Rect (pos x) (pos y) fill fill
              pos n = padding + (n - 1) * size
              fill = size - border

legalNeighbour :: (Int, Int) -> [(Int, Int)] -> Level -> Bool
legalNeighbour (x,y) clist level =
                (lMap level ! (x,y) == Full)
                    && not (null clist)
                    && notElem (x,y) clist
                    && neighbour (x,y)  (head clist)
                where neighbour (x,y) (x',y') = elem (x,y) [(x'+1,y')
                                                           ,(x'-1,y')
                                                           ,(x',y'+1)
                                                           ,(x',y'-1)]

legalMove :: (Int, Int) -> [(Int, Int)] -> Level -> (Bool, [(Int, Int)])
legalMove (x,y) clist level =
                   if elem (x,y)  (indices (lMap level))
                      && legalNeighbour (x,y) clist level
                    || null clist
                       && (x,y) == lStart level
                   then (True, (x,y):clist)
                   else (False, clist)

position :: Int -> Int -> (Int,Int)
position curX curY = (x+1,y+1)
     where x = (curX-padding) `div` cellsize
           y = (curY-padding) `div` cellsize

eventHandler ::  GameState -> IO ()
eventHandler gamestate = do
  e <- waitEvent
  case e of
    Quit -> return ()
    KeyDown (Keysym key _ _)   ->
        case key of
            SDLK_q      -> return ()
            SDLK_ESCAPE -> return ()
            _           -> eventHandler gamestate
    MouseMotion x y _ _ -> do
                  let pos = position (fI x) (fI y)
                  let (en,cList') = legalMove pos (cList gamestate) currentlevel
                  if en then do 
                    screen <- SDL.getVideoSurface
                    drawStep screen cList'
                    SDL.flip screen
                    if length cList' == lLength currentlevel
                         then unless (null remaininglvls) $ start remaininglvls
                         else eventHandler gamestate {cList = cList'}
                    else eventHandler gamestate
                  where fI = fromIntegral
                        remaininglvls = tail . levels $ gamestate
                        currentlevel = head . levels $ gamestate
    MouseButtonDown _ _ _  -> start $ levels gamestate
    _ -> eventHandler gamestate

drawStep screen clist = 
    if length clist > 1
      then do draw screen hlcolor (clist !! 1)
              draw screen actcolor $ head clist
      else draw screen actcolor $ head clist
