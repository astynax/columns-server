-- | Game server

module Columns.Server
  ( runApp
  ) where

import           Control.Monad.Reader
import           Control.Monad.Trans             (liftIO)
import           Data.IORef                      (modifyIORef', newIORef,
                                                  readIORef)
import           Network.Wai                     (Application)
import           Network.Wai.Cli                 (defWaiMain)
import           Network.Wai.Middleware.HttpAuth
import qualified Web.Scotty.Trans                as S

import           Columns.Game
import           Columns.Render                  (css, renderGame)
import           Columns.Server.Auth
import           Columns.Server.Types

scottyApp :: ServerM ()
scottyApp = do
  S.get "/" $ do
    ref <- getRef
    game <- liftIO $ readIORef ref
    S.html $ renderGame game

  S.get "/click/:x/:y" $ do
    x <- S.param "x"
    y <- S.param "y"
    ref <- getRef
    liftIO $ modifyIORef' ref $ \game ->
      game{board = update2d x y (const $ Just (First, D1))
          $ board game}
    S.redirect "/"

  S.get "/styles.css" $ do
    S.addHeader "Content-Type" "text/css"
    S.text css

  where
    getRef = lift $ asks getEnv

mkApp :: IO Application
mkApp = do
  ref <- newIORef $ Game First "43..\n21..\n..ab\n..cd"
  S.scottyAppT (`runReaderT` Env ref) scottyApp

runApp :: IO ()
runApp = defWaiMain . middlewares =<< mkApp
  where
    middlewares =
      basicAuth
        checkPassword
        "Columns"
