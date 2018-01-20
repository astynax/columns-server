-- | Limited range numbers

module Columns.Game.Types.N4 where

data N4 tag
  = D1
  | D2
  | D3
  | D4
  deriving (Show, Eq, Ord, Enum)

toInt :: N4 t -> Int
toInt D1 = 1
toInt D2 = 2
toInt D3 = 3
toInt D4 = 4
