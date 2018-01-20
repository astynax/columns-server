-- | Fixed size arrays

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}

module Columns.Game.Types.Array
  ( Array4(..)
  , X, Y, Array2d
  , get, put, update
  , toIndexedList
  ) where

import           Data.Foldable

import           Columns.Game.Types.N4

data Array4 tag a =
  Array4 a a a a
  deriving (Show, Functor, Traversable, Foldable)

data X
data Y
type Array2d a = Array4 Y (Array4 X a)

get :: N4 t -> Array4 t a -> a
get D1 (Array4 x _ _ _) = x
get D2 (Array4 _ x _ _) = x
get D3 (Array4 _ _ x _) = x
get D4 (Array4 _ _ _ x) = x

put :: N4 t -> a -> Array4 t a -> Array4 t a
put D1 x (Array4 _ b c d) = Array4 x b c d
put D2 x (Array4 a _ c d) = Array4 a x c d
put D3 x (Array4 a b _ d) = Array4 a b x d
put D4 x (Array4 a b c _) = Array4 a b c x

update :: N4 t -> (a -> a) -> Array4 t a -> Array4 t a
update i f a = put i (f $ get i a) a

toIndexedList :: Array4 t a -> [(N4 t, a)]
toIndexedList = zip [D1 ..] . toList
