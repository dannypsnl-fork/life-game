module Main where

import Control.Concurrent
import LifeGame
import System.Environment
import qualified UI.HSCurses.Curses as Curses

main :: IO ()
main = do
  args <- getArgs
  case args of
    (wS : hS : _) -> do
      let size = (read wS :: Int, read hS :: Int)
      field <- randomField size
      Curses.initCurses
      gameStart field size $ \field -> do
        Curses.erase
        let fieldStr = show field
        Curses.wAddStr Curses.stdScr fieldStr
        Curses.refresh
        threadDelay 30000
      Curses.endWin
    _ -> putStrLn "Specify size of the field"

  return ()
