module LifeGame (randomField, gameStart) where

import Control.Monad
import LifeGame.Cell
import LifeGame.Field
import LifeGame.Next
import System.Random

type Size = (Int, Int)

instance Show Field where
  show (Field []) = ""
  show (Field (row : rows)) = foldl (\s c -> s ++ show c) "" row ++ "\n" ++ show (Field rows)

gameStart :: Field -> Size -> (Field -> IO ()) -> IO ()
gameStart field (w, h) action = do
  action field
  let next = wrapWall (w, h) $ [[nextCell field (i, j) | j <- [0 .. w -1]] | i <- [0 .. h -1]]
   in gameStart next (w, h) action

randomField :: Size -> IO Field
randomField (w, h) = do
  rows <- forM [0 .. h] $ \_ -> do
    take w . randoms <$> newStdGen
  return $ wrapWall (w, h) rows

wrapWall :: Size -> [[Cell]] -> Field
wrapWall (w, h) rows =
  Field $ wallRow : map (\row -> Wall : row ++ [Wall]) rows ++ [wallRow]
  where
    wallRow = WallVH : replicate w WallV ++ [WallVH]
