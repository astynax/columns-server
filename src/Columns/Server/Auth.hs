module Columns.Server.Auth
  ( registerUser
  , checkPassword
  ) where

import           Control.Exception     (finally)
import           Control.Monad         (void)
import           Control.Monad.Trans   (liftIO)
import qualified Crypto.BCrypt         as BC
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T
import           Database.MongoDB

checkPassword :: ByteString -> ByteString -> IO Bool
checkPassword user pass = withConn "columns" $ do
  r <- find
    (select ["name" =: userText] "users")
    >>= rest
  case r of
    [u] ->
      case u !? "pass" of
        Just hash ->
          let hashBS = BS.pack . T.unpack $ hash
          in pure $ BC.validatePassword hashBS pass
        Nothing   ->
          fail "No password in record!"
    _   -> pure False
  where
    userText = T.pack . BS.unpack $ user

-- | Registers a new user (suitable for REPL)
registerUser
  :: ByteString  -- ^ username
  -> ByteString  -- ^ password
  -> IO Bool
registerUser user pass = withConn "columns" $ do
  let userText = T.pack . BS.unpack $ user
  mbHashBS <-
    liftIO $ BC.hashPasswordUsingPolicy BC.fastBcryptHashingPolicy pass
  case mbHashBS of
    Nothing     -> pure False
    Just hashBS -> do
      insert "users"
        [ "name" =: userText
        , "pass" =: (T.pack . BS.unpack $ hashBS) ]
      pure True

withConn :: Database -> Action IO b -> IO b
withConn dbName action = do
  pipe <- connect $ host "127.0.0.1"
  access pipe master dbName action
    `finally` close pipe
