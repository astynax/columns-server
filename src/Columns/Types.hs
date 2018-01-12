{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}

module Columns.Types
  ( Game(..)
  , Player(..)
  , Board, Cell
  , Array4(..)
  , Array2d
  , X, Y, Piece
  , newGame
  , update2d
  , module Columns.Types.N4
  ) where

import Data.Function
import Data.String

import Columns.Types.N4

data Game = Game
  { currentPlayer :: Player
  , board :: Board
  } deriving (Show)

data Player
  = First
  | Second
  deriving (Eq, Show)

type Board = Array2d Cell

type Cell = Maybe (Player, N4 Piece)

data X
data Y
data Piece

data Array4 tag a =
  Array4 a a a a
  deriving (Show, Functor, Traversable, Foldable)

type Array2d a = Array4 Y (Array4 X a)

newGame :: Game
newGame = Game First b
  where
    fill x = Array4 x x x x
    empty = fill (fill Nothing)
    f = Just (First, D1)
    s = Just (Second, D1)
    b = empty
      & update2d D1 D1 (const f)
      & update2d D2 D1 (const f)
      & update2d D3 D1 (const f)
      & update2d D4 D1 (const f)
      & update2d D1 D4 (const s)
      & update2d D2 D4 (const s)
      & update2d D3 D4 (const s)
      & update2d D4 D4 (const s)

getA4 :: N4 t -> Array4 t a -> a
getA4 D1 (Array4 x _ _ _) = x
getA4 D2 (Array4 _ x _ _) = x
getA4 D3 (Array4 _ _ x _) = x
getA4 D4 (Array4 _ _ _ x) = x

putA4 :: N4 t -> a -> Array4 t a -> Array4 t a
putA4 D1 x (Array4 _ b c d) = Array4 x b c d
putA4 D2 x (Array4 a _ c d) = Array4 a x c d
putA4 D3 x (Array4 a b _ d) = Array4 a b x d
putA4 D4 x (Array4 a b c _) = Array4 a b c x

updateA4 :: N4 t -> (a -> a) -> Array4 t a -> Array4 t a
updateA4 i f a = putA4 i (f $ getA4 i a) a

update2d :: N4 X -> N4 Y -> (a -> a) -> Array2d a -> Array2d a
update2d x y = updateA4 y . updateA4 x

instance IsString Board where
  fromString s =
    let (a:b:c:d:[]) = map toArray $ lines s
    in Array4 a b c d
    where
      toArray xs =
        let (a:b:c:d:[]) = map toCell xs
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
