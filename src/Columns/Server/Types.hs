-- | Server types

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Columns.Server.Types where

import           Control.Monad.Reader
import           Data.IORef           (IORef)
import           Data.Text.Lazy       (Text)
import           Web.Scotty.Trans     (ActionT, Parsable(..), ScottyT)

import           Columns.Game

newtype Env = Env
  { getEnv :: IORef Game
  }

type ServerM = ScottyT Text (ReaderT Env IO)

type HandlerM = ActionT Text (ReaderT Env IO)

instance Parsable (N4 t) where
  parseParam "1" = Right D1
  parseParam "2" = Right D2
  parseParam "3" = Right D3
  parseParam "4" = Right D4
  parseParam _   = Left "Bad number"
