{-# LANGUAGE OverloadedStrings #-}
module Auth where

import Data.Aeson hiding (Success)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)

import Types

data AuthReq = AuthReq {
  arLogin :: T.Text
} deriving (Show)

instance FromJSON AuthReq where
  parseJSON (Object v) = AuthReq <$> v .: "login"

data AuthStatus = Success | AlreadyExists deriving (Show, Eq)

stringify :: AuthStatus -> T.Text
stringify Success = "success"
stringify AlreadyExists = "alreadyExists"

data AuthRes = AuthRes {
  arStatus :: AuthStatus
} deriving (Show)

instance ToJSON AuthRes where
  toJSON (AuthRes { arStatus = st }) = object ["status" .= stringify st]

auth :: AuthReq -> TVar ServerState -> IO AuthRes
auth (AuthReq { arLogin = login }) st = atomically $ do
  s@(S { users = us }) <- readTVar st
  if any (== login) us
  then return $ AuthRes { arStatus = AlreadyExists }
  else do
    writeTVar st $ s { users = login:us }
    return $ AuthRes { arStatus = Success }

isSuccess :: AuthRes -> Bool
isSuccess (AuthRes {arStatus = Success}) = True
isSuccess _ = False

data AuthStatusRes = AuthStatusRes {
  asrLogin :: Maybe T.Text
} deriving (Show)

instance ToJSON AuthStatusRes where
  toJSON (AuthStatusRes { asrLogin = Nothing }) = object ["status" .= T.pack "unknown"]
  toJSON (AuthStatusRes { asrLogin = Just login }) =
    object [
      "status" .= T.pack "authorized",
      "login" .= login
    ]

getStatus :: Maybe T.Text -> TVar ServerState -> IO AuthStatusRes
getStatus Nothing _ = return $ AuthStatusRes { asrLogin = Nothing }
getStatus (Just login) st = atomically $ do
  S { users = us } <- readTVar st
  if any (== login) us
  then return $ AuthStatusRes { asrLogin = Just login }
  else return $ AuthStatusRes { asrLogin = Nothing }

logout :: T.Text -> TVar ServerState -> IO ()
logout login st = atomically $ do
  s@(S { users = us }) <- readTVar st
  writeTVar st $ s { users = filter (/= login) us }
