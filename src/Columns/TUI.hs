module Columns.TUI where

import Data.Foldable

import Columns.Types

printGame :: Game -> IO ()
printGame (Game _ b) = do
  putStrLn "   1  2  3  4"
  hl
  mapM_ printRow $ zip "ABCD" $ toList b
  where
    hl = putStrLn "  +--+--+--+--+"
    printRow (l, r) = do
      putChar l
      putStr " |"
      mapM_ (printCell True) r
      putStrLn ""
      putStr "  |"
      mapM_ (printCell False) r
      putStrLn ""
      hl
    printCell _ Nothing = putStr "  |"
    printCell x (Just (p, d)) = do
      putStr $ case (x, d) of
        (False, D1) -> c:" "
        (True,  D3) -> c:" "
        (True,  D4) -> c:c:""
        _  | x      -> "  "
        _           -> c:c:""
      putChar '|'
      where
        c = case p of
          First  -> '1'
          Second -> '2'
