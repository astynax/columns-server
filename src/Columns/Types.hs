{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}

module Columns.Types where

import Data.Function
import Data.String

data Player
  = First
  | Second
  deriving (Eq, Show)

data D4 = D1 | D2 | D3 | D4 deriving (Show, Eq, Ord)

data X
data Y

newtype Coord t = Coord D4 deriving (Show, Eq, Ord)

data Array4 t a =
  Array4 a a a a
  deriving (Show, Functor, Traversable, Foldable)

type Array2d a = Array4 Y (Array4 X a)

type Cell = Maybe (Player, D4)

type Board = Array2d Cell

data Game = Game
  { currentPlayer :: Player
  , board :: Array2d Cell
  } deriving (Show)

newGame :: Game
newGame = Game First b
  where
    fill x = Array4 x x x x
    empty = fill (fill Nothing)
    f = Just (First, D1)
    s = Just (Second, D1)
    b = empty
      & update2d (Coord D1) (Coord D1) (const f)
      & update2d (Coord D1) (Coord D2) (const f)
      & update2d (Coord D1) (Coord D3) (const f)
      & update2d (Coord D1) (Coord D4) (const f)
      & update2d (Coord D4) (Coord D1) (const s)
      & update2d (Coord D4) (Coord D2) (const s)
      & update2d (Coord D4) (Coord D3) (const s)
      & update2d (Coord D4) (Coord D4) (const s)

getA4 :: Coord t -> Array4 t a -> a
getA4 (Coord D1) (Array4 x _ _ _) = x
getA4 (Coord D2) (Array4 _ x _ _) = x
getA4 (Coord D3) (Array4 _ _ x _) = x
getA4 (Coord D4) (Array4 _ _ _ x) = x

putA4 :: Coord t -> a -> Array4 t a -> Array4 t a
putA4 (Coord D1) x (Array4 _ b c d) = Array4 x b c d
putA4 (Coord D2) x (Array4 a _ c d) = Array4 a x c d
putA4 (Coord D3) x (Array4 a b _ d) = Array4 a b x d
putA4 (Coord D4) x (Array4 a b c _) = Array4 a b c x

updateA4 :: Coord t -> (a -> a) -> Array4 t a -> Array4 t a
updateA4 i f a = putA4 i (f $ getA4 i a) a

update2d :: Coord X -> Coord Y -> (a -> a) -> Array2d a -> Array2d a
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
