module Columns.Server ( runApp ) where

import           Control.Monad.Reader
import           Control.Monad.Trans             (liftIO)
import           Data.IORef                      (IORef, modifyIORef', newIORef,
                                                  readIORef)
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as T
import qualified Network.HTTP.Types              as HTTP
import           Network.Wai                     (Application)
import           Network.Wai.Cli                 (defWaiMain)
import           Network.Wai.Middleware.HttpAuth
import           Web.Scotty.Trans                (ActionT, ScottyT)
import qualified Web.Scotty.Trans                as S

import           Columns.Render                  (css, renderGame)
import           Columns.Server.Auth
import           Columns.Types

newtype Env = Env
  { getEnv :: IORef Game
  }

type ScottyMonad = ScottyT Text (ReaderT Env IO)

scottyApp :: ScottyMonad ()
scottyApp = do
  S.get "/" $ do
    ref <- getRef
    game <- liftIO $ readIORef ref
    S.html $ renderGame game

  S.get "/click/:x/:y" $ do
    mx <- fromText <$> S.param "x"
    my <- fromText <$> S.param "y"
    case (mx, my) of
      (Just x, Just y) -> do
        ref <- getRef
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
