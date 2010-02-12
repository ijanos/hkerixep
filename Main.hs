module Main () where

import Data.Array
import Data.List
import Graphics.UI.SDL as SDL
import Control.Applicative

import Levels

cellsize  = 40
border    = 1
padding   = 30
wwidth    = 450
wheight   = 450
cellcolor = SDL.Pixel 0x00999999
hlcolor   = SDL.Pixel 0x00991111
actcolor  = SDL.Pixel 0x00118505

main ::  IO ()
main = do SDL.init [SDL.InitVideo]
          setVideoMode wwidth wheight 32 [HWSurface, DoubleBuf]
          setCaption "hKerixep" []
          level1 <- last <$> getLevels -- this just quick and dirty
          start level1

start level = do screen<- SDL.getVideoSurface
                 mapM_ (draw screen cellcolor)
                       [(x,y)|((x,y),Full) <- assocs (lMap level)]
                 draw screen actcolor (lStart level)
                 SDL.flip screen
                 eventHandler [lStart level] level

draw ::  Surface -> Pixel -> (Int, Int) -> IO Bool
draw screen color (x,y) = SDL.fillRect screen (Just rect) color
        where rect = SDL.Rect (pos x) (pos y) fill fill
              pos n = padding + (n - 1) * cellsize
              fill = cellsize - border

legalmove (x, y) clist@(c:cs) level =
                   if elem (x,y)  (indices (lMap level))
                      && (lMap level ! (x,y) == Full)
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

eventHandler clist level = do
  e <- waitEvent
  case e of
    Quit -> return ()
    KeyDown (Keysym key _ _)   ->
        case key of
            SDLK_q      -> return ()
            SDLK_ESCAPE -> return ()
            _           -> eventHandler clist level
    MouseMotion x y _ _ -> do let pos = position (fI x) (fI y)
                              let (en,newclist) = legalmove pos clist level
                              if en then do screen <- SDL.getVideoSurface
                                            draw screen hlcolor $ head clist
                                            draw screen actcolor pos
                                            SDL.flip screen
                                            if length newclist == lLength level
                                             then do print "A WINNER IS YOU"
                                             else eventHandler newclist level
                                    else eventHandler clist level
                              where fI = fromIntegral
    MouseButtonDown _ _ _  -> start level
    _ -> eventHandler clist level
