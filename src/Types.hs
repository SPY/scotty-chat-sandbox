module Types where

import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Data.Unique

data ServerState = S {
  users :: [T.Text],
  connections :: [(Unique, WS.Connection)]
}

