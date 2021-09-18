module LifeGame.Field (Field (..)) where

import LifeGame.Cell

newtype Field = Field {runField :: [[Cell]]}
