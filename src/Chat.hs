{-# LANGUAGE OverloadedStrings #-}
module Chat where

import Data.Aeson hiding (Success)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad (forM_)

import Types

data ChatEvent = Enter T.Text
               | Leave T.Text
               | Message T.Text T.Text
               deriving (Show)

instance ToJSON ChatEvent where
  toJSON (Enter login) = object ["event" .= ("enter" :: T.Text), "user" .= login]
  toJSON (Leave login) = object ["event" .= ("leave" :: T.Text), "user" .= login]
  toJSON (Message login msg) = object ["event" .= ("message" :: T.Text), "user" .= login, "message" .= msg]

data MessageReq = MessageReq {
  mrText :: T.Text
}

instance FromJSON MessageReq where
  parseJSON (Object v) = MessageReq <$> v .: "message"

sendToAll :: TVar ServerState -> ChatEvent -> IO ()
sendToAll st ev = do
  let msg = encode ev
  S { connections = conns } <- atomically $ readTVar st
  forM_ conns $ flip WS.sendTextData msg . snd
