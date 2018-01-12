module Columns.Server ( runApp ) where

import           Control.Monad.Trans (liftIO)
import           Data.IORef          (IORef, readIORef, modifyIORef', newIORef)
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T
import qualified Network.HTTP.Types  as HTTP
import           Network.Wai         (Application)
import           Network.Wai.Cli     (defWaiMain)
import qualified Web.Scotty          as S

import           Columns.Render      (css, renderGame)
import           Columns.Types

app' :: IORef Game -> S.ScottyM ()
app' ref = do
  S.get "/" $ do
    game <- liftIO $ readIORef ref
    S.html $ renderGame $ Game First "43..\n21..\n..ab\n..cd"

  S.get "/click/:x/:y" $ do
    mx <- fromText <$> S.param "x"
    my <- fromText <$> S.param "y"
    case (mx, my) of
      (Just x, Just y) -> do
        liftIO $ modifyIORef' ref $ \game ->
          game{board = update2d x y (const $ Just (First, D1))
              $ board game}
        S.redirect "/"
      _ -> do
        S.status HTTP.badRequest400
        S.text "Bad request!"

  S.get "/styles.css" $ do
    S.addHeader "Content-Type" "text/css"
    S.text css

app :: IO Application
app = do
  ref <- newIORef newGame
  S.scottyApp $ app' ref

runApp :: IO ()
runApp = defWaiMain =<< app
