module LifeGame (randomField, gameStart) where

import Control.Monad
import LifeGame.Cell
import System.Random

newtype Field = Field {runField :: [[Cell]]}

type Pos = (Int, Int)

type Size = (Int, Int)

at :: Field -> Pos -> Cell
Field field `at` (i, j) = field !! (i + 1) !! (j + 1)

instance Show Field where
  show (Field []) = ""
  show (Field (row : rows)) = foldl (\s c -> s ++ show c) "" row ++ "\n" ++ show (Field rows)

gameStart :: Field -> Size -> (Field -> IO ()) -> IO ()
gameStart field (w, h) action = do
  action field
  let next = wrapWall (w, h) $ [[nextCell field (i, j) | j <- [0 .. w -1]] | i <- [0 .. h -1]]
   in gameStart next (w, h) action

aliveCntAround :: Field -> Pos -> Int
aliveCntAround field (i, j) = foldl (\s e -> if e == Alive then s + 1 else s) 0 $ map (field `at`) [(i -1, j -1), (i -1, j), (i -1, j + 1), (i, j -1), (i, j + 1), (i + 1, j -1), (i + 1, j), (i + 1, j + 1)]

nextCell :: Field -> Pos -> Cell
nextCell field pos
  | nowCell == Empty && aroundCnt == 3 = Alive
  | nowCell == Alive && elem aroundCnt [2, 3] = Alive
  | nowCell == Alive && aroundCnt <= 1 = Empty
  | nowCell == Alive && aroundCnt >= 4 = Empty
  | otherwise = Empty
  where
    nowCell = field `at` pos
    aroundCnt = aliveCntAround field pos

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