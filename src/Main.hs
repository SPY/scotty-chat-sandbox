{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import qualified Network.Wai.Middleware.RequestLogger as Logger
import qualified Network.Wai.Middleware.Static as Static
import qualified Data.Text.Lazy.IO as T
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))

import qualified Auth

main :: IO ()
main = scotty 1488 $ do
  middleware Logger.logStdout

  post "/api/auth" $ do
    login <- Auth.arLogin <$> jsonData
    liftIO $ putStrLn $ "Login request from " ++ login
    json $ Auth.AuthRes $ if login == "spy" then Auth.AlreadyExists else Auth.Success

  frontend "./frontend"

frontend :: String -> ScottyM ()
frontend appDir = do
  middleware $ Static.staticPolicy $ Static.addBase appDir
  get "/" $ (liftIO $ T.readFile $ appDir ++ "/index.html") >>= html
