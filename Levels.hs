module Levels ( Level (lStart, lLength, lMap)
              , Cell (Full, Empty)
              , getLevels
              ) where

import Data.List
import Data.Array
import Control.Applicative

data Level = Level {
        lStart  :: (Int,Int),
        lLength :: Int,
        lMap    :: Array (Int,Int) Cell
    } deriving (Show)

data Cell = Full | Empty
        deriving (Eq, Show)

levelsep _ chr = case chr of
                     "" -> False
                     _  -> True

getLevels file = do
        lines <- lines <$> readFile file
        let levels' = groupBy levelsep lines
        let levels'' = (map (dropWhile null) levels')
        return $ map generate levels''
        where generate level = Level lstart llength lmap
                               where lstart = getStart level
                                     lmap = convert level
                                     llength = length [ x | x@Full <- elems lmap]

convert level = listArray ((1,1),(w,h)) [conv x | x <- concat.transpose $ level]
    where h = length level
          w = length $ head level
          conv ch = case ch of
              '#' -> Full
              'S' -> Full
              _   -> Empty

getStart level = case foldl f (Left (1,1)) level of
                    Right coords -> coords
                    Left _      ->  error "Cannot find start position"
        where f = \acc str -> case acc of
                                Left (x,y) -> case elemIndex 'S' str of
                                                Just i -> Right (x',y)
                                                          where x' = i+1
                                                Nothing -> Left (x,y')
                                                           where y' = y + 1
                                Right a -> Right a
