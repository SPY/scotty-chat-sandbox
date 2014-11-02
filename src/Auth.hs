{-# LANGUAGE OverloadedStrings #-}
module Auth where

import Data.Aeson hiding (Success)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text.Lazy as T

data AuthReq = AuthReq {
     arLogin :: String
     }
     deriving (Show)

instance FromJSON AuthReq where
  parseJSON (Object v) = AuthReq <$> v .: "login"

data AuthStatus = Success | AlreadyExists deriving (Show)

stringify :: AuthStatus -> T.Text
stringify Success = "success"
stringify AlreadyExists = "alreadyExists"

data AuthRes = AuthRes {
     arStatus :: AuthStatus
     }
     deriving (Show)

instance ToJSON AuthRes where
  toJSON (AuthRes { arStatus = st }) = object ["status" .= stringify st]

