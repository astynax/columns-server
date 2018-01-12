module Columns.Render
  ( css
  , renderGame
  ) where

import qualified Clay
import           Control.Monad                 (when)
import           Data.Foldable                 (toList)
import           Data.Monoid                   ((<>))
import           Data.String                   (fromString)
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as T
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (Html, (!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

import           Columns.Render.Styles
import           Columns.Types

css :: Text
css = Clay.render styles

renderGame :: Game -> Text
renderGame (Game _ b) = renderHtml $ do
  H.head $
    H.link ! A.rel "stylesheet" ! A.href "/styles.css"
  H.body $
    H.table ! A.id "board" $
      H.tbody $
        mapM_ renderRow $ addIdx b
  where
    renderRow (y, r) =
      H.tr $ mapM_ renderTD $ addIdx r
      where
        renderTD (x, v) = td_ (x, y) $ renderCell v
    td_ (x, y) content =
      H.td $
        H.a ! A.href url $
          H.div ! A.class_ ("board-cell " <> c)
            $ content
      where
        url =
          "/click/"
          <> fromString (show $ toInt x)
          <> "/"
          <> fromString (show $ toInt y)
        c | odd (toInt x + toInt y) = "white"
          | otherwise               = "black"

addIdx :: Array4 t a -> [(N4 t, a)]
addIdx = zip [D1, D2, D3, D4] . toList

renderCell :: Cell -> Html
renderCell Nothing       = H.text ""
renderCell (Just (p, v)) = do
  when (v == D4) piece
  when (v >= D3) piece
  when (v >= D2) piece
  when (v >= D1) piece
  where
    piece = H.div ! A.class_ ("piece " <> c) $ pure ()
    c | p == First = "red"
      | otherwise  = "blue"
