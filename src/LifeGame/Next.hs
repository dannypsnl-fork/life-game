module LifeGame.Next (nextCell) where

import LifeGame.Cell
import LifeGame.Field

type Pos = (Int, Int)

at :: Field -> Pos -> Cell
Field field `at` (i, j) = field !! (i + 1) !! (j + 1)

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
