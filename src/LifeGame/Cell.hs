module LifeGame.Cell (Cell (..)) where

import System.Random

data Cell
  = Alive
  | Empty
  | Wall
  | WallV
  | WallVH
  deriving (Bounded, Eq, Enum)

instance Show Cell where
  show Alive = "*"
  show Empty = " "
  show Wall = "|"
  show WallV = "-"
  show WallVH = "+"

add1 :: Int -> Int
add1 n = n + 1

instance Random Cell where
  random g = case randomR (fromEnum (minBound :: Cell), add1 $ fromEnum (minBound :: Cell)) g of
    (r, g') -> (toEnum r, g')
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')
