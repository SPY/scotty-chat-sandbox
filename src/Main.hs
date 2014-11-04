{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Web.Scotty.Cookie
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.WebSockets as WS
import Network.WebSockets (ConnectionException, ServerApp, pendingRequest, requestPath, rejectRequest, defaultConnectionOptions, acceptRequest, receiveData)
import qualified Network.Wai.Middleware.Static as Static
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Control.Exception (handle)
import Data.Unique

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)

import qualified Auth
import qualified Chat
import Types

main :: IO ()
main = do
  let settings = W.setPort 1488 W.defaultSettings
  state <- liftIO $ atomically $ newTVar $ S { users = [], connections = [] }
  liftIO $ T.putStrLn "start server at http://localhost:1488"
  app <- scottyApp $ do
    post "/api/auth" $ do
      req <- jsonData
      liftIO $ putStrLn $ "Login request from " ++ (show req)
      res <- liftIO $ Auth.auth req state
      when (Auth.isSuccess res) $ do
        setSimpleCookie "login" $ Auth.arLogin req
      json res

    get "/api/status" $ do
      res <- getCookie "login" >>= (liftIO . flip Auth.getStatus state)
      json res

    get "/api/logout" $ do
      deleteCookie "login"
      login <- getCookie "login"
      when (isJust login) $ liftIO $ Auth.logout (fromJust login) state
      text "{\"status\":\"ok\"}"

    get "/api/users" $ do
      users <- liftIO $ atomically $ do
        S { users = us } <- readTVar state
        return us
      text $ TL.pack $ show users

    frontend "./frontend"
  let ws = WS.websocketsOr defaultConnectionOptions (onWebSocket state) app
  W.runSettings settings ws

frontend :: String -> ScottyM ()
frontend appDir = do
  middleware $ Static.staticPolicy $ Static.addBase appDir
  get "/" $ (liftIO $ T.readFile $ appDir ++ "/index.html") >>= html

onWebSocket :: TVar ServerState -> ServerApp
onWebSocket st pending = do
  let path = requestPath . pendingRequest $ pending
  let (prefix, suffix) = BS.splitAt 6 path
  if prefix == "/chat/" && BS.length suffix > 0
  then do
    let login = decodeUtf8 suffix
    -- non consistent
    s@(S { users = us, connections = conns }) <- atomically $ readTVar st
    if login `elem` us
    then do
      conn <- acceptRequest pending
      uniq <- newUnique
      atomically $ writeTVar st $ s { connections = (uniq, conn) : conns }
      Chat.sendToAll st $ Chat.Enter login
      handle (catchDisconect st (login, uniq)) $ forever $ do
        msg <- Aeson.decode <$> receiveData conn
        case msg of
          Just (Chat.MessageReq msgText) ->
            Chat.sendToAll st $ Chat.Message login msgText
          Nothing ->
            return ()
    else rejectRequest pending "Such user is not exists"
  else do
    rejectRequest pending "Such user is not exists"
  where
    catchDisconect :: TVar ServerState -> (T.Text, Unique) -> ConnectionException -> IO ()
    catchDisconect st (login, connId) _ex = do
      atomically $ do
        s@(S { connections = conns }) <- readTVar st
        writeTVar st $ s { connections = filter ((/= connId) . fst) conns }
      putStrLn $ "Disconect ws"
      Chat.sendToAll st $ Chat.Leave login
