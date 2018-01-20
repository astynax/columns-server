-- | Game logic types

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Columns.Game.Types
  ( Game(..)
  , Player(..)
  , Board, Cell, Piece
  , newGame
  , update2d
  , module Reexports
  ) where

import           Data.String

import           Columns.Game.Types.Array
import           Columns.Game.Types.Array as Reexports (X, Y)
import           Columns.Game.Types.N4    as Reexports

data Game = Game
  { currentPlayer :: Player
  , board         :: Board
  } deriving (Show)

data Player
  = First
  | Second
  deriving (Eq, Show)

type Board = Array2d Cell

type Cell = Maybe (Player, N4 Piece)

data Piece

newGame :: Game
newGame = Game First "1111\n....\n....\naaaa"

update2d :: N4 X -> N4 Y -> (a -> a) -> Array2d a -> Array2d a
update2d x y = update y . update x

instance IsString Board where
  fromString s =
    let [a, b, c, d] = map toArray $ lines s
    in Array4 a b c d
    where
      toArray xs =
        let [a, b, c, d] = map toCell xs
        in Array4 a b c d
      toCell '1' = Just (First, D1)
      toCell '2' = Just (First, D2)
      toCell '3' = Just (First, D3)
      toCell '4' = Just (First, D4)
      toCell 'a' = Just (Second, D1)
      toCell 'b' = Just (Second, D2)
      toCell 'c' = Just (Second, D3)
      toCell 'd' = Just (Second, D4)
      toCell _   = Nothing
