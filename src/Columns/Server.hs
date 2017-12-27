module Columns.Server (runApp, app, printGame) where

import           Network.Wai (Application)
import           Network.Wai.Cli
import qualified Web.Scotty as S

import           Columns.Types

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.text "hello"

  S.get "/some-json" $ do
    S.text "nope"

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = defWaiMain =<< app

printGame :: Game -> IO ()
printGame (Game _ b) =
  mapM_ printRow b
  where
    printRow r = mapM_ printCell r >> putStrLn ""
    printCell Nothing = putStr " . "
    printCell (Just (p, d)) = do
      case p of
        First -> putChar '<'
        Second -> putChar '['
      case d of
        D1 -> putChar '1'
        D2 -> putChar '2'
        D3 -> putChar '3'
        D4 -> putChar '4'
      case p of
        First -> putChar '>'
        Second -> putChar ']'
