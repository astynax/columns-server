module Columns.Types.N4 where

import Data.Text

data N4 tag
  = D1
  | D2
  | D3
  | D4
  deriving (Show, Eq, Ord)

fromText :: Text -> Maybe (N4 t)
fromText "1" = Just D1
fromText "2" = Just D2
fromText "3" = Just D3
fromText "4" = Just D4
fromText _   = Nothing

toInt :: N4 t -> Int
toInt D1 = 1
toInt D2 = 2
toInt D3 = 3
toInt D4 = 4
