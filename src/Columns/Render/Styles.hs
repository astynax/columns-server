module Columns.Render.Styles where

import           Clay

styles :: Css
styles = do
  "#board" ? do
    borderSpacing nil
    borderCollapse separate

    "td" ?
      padding nil nil nil nil

  ".board-cell" & do
    width $ px 50
    height $ px 50

    display tableCell
    verticalAlign middle

    ".white" & do
      backgroundColor $ grayish 240

    ".black" &
      backgroundColor black

  ".piece" & do
    width $ px 40
    height $ px 8
    margin nil auto nil auto

    border outset (px 1) grey

    ".red" &
      backgroundColor red

    ".blue" &
      backgroundColor blue
