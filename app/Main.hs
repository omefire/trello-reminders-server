{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified DB as DB
import Servant.API
import Servant.Server
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import qualified Data.Text as T
import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as BSLI
import qualified Text.Email.Validate as Email
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import ConnectionInfo as CI
import Control.Monad.Except (throwError)
import Types
import Data.String.Interpolate
import Control.Concurrent


-- http://localhost:8081/getEmailsForUser/omefire@gmail.com
type EmailAPI = "getEmailsForUser" :> Capture "UserID" UserID :> Get '[JSON] [Email]
type ReminderAPI = "createReminder" :> ReqBody '[JSON] Reminder :> Post '[JSON] Int
type UserAPI = "getUserIDForEmail" :> Capture "Email" String :> Get '[JSON] (Maybe UserID)
type TrelloTokenAPI = "setTrelloToken" :> ReqBody '[JSON] TrelloToken :> Post '[JSON] (String, String)
                 :<|> "getTrelloToken" :> Capture "TrelloID" String :> Get '[JSON] String

type API = EmailAPI :<|> ReminderAPI :<|> UserAPI :<|> TrelloTokenAPI

-- TODO: Security: How to prevent people from getting other accounts' emails by spoofing their account's email?
-- TODO: Remove code duplication
server :: Server API
server = getEmailsForUser :<|> createReminder :<|> getUserIDForEmail :<|> setTrelloToken :<|> getTrelloToken
  where getEmailsForUser :: UserID -> Handler [Email]
        getEmailsForUser userid = do
          eConnInfo <- liftIO $ CI.getConnectionInfo
          case eConnInfo of
            Left err -> throwError err505 { errBody = BLC.pack err }
            Right connInfo -> do
              conn <- liftIO $ connect ConnectInfo {connectHost = host connInfo
                                                   ,connectPort = (fromIntegral $ port connInfo)
                                                   ,connectDatabase = database connInfo
                                                   ,connectPassword = password connInfo
                                                   ,connectUser = user connInfo
                                                   }
              emls <- liftIO $ DB.getEmailsForUser conn userid
              return  emls

        --
        -- Example JSON body request
        -- {
	--  "reminderID": 0,
	--  "reminderName": "Reminder Name",
	--  "reminderDescription": "Reminder Description",
	--  "reminderDateTime": "2016-12-09T15:04:26.349857693845+05:00",
	--  "reminderEmails": [ { "emailID": 1, "emailValue": "omefire@gmail.com"}],
	--  "reminderUserID": 1
        -- }
        createReminder :: Reminder -> Handler Int
        createReminder rem = do
          eConnInfo <- liftIO $ CI.getConnectionInfo
          case eConnInfo of
            Left err -> throwError err505 { errBody = BLC.pack err }
            Right connInfo -> do
              conn <- liftIO $ connect ConnectInfo {connectHost = host connInfo
                                                   ,connectPort = (fromIntegral $ port connInfo)
                                                   ,connectDatabase = database connInfo
                                                   ,connectUser = user connInfo
                                                   ,connectPassword = password connInfo
                                                   }
              let connString = [i|host='#{host connInfo}' dbname='#{database connInfo}' user='#{user connInfo}' password='#{password connInfo}' port='#{port connInfo}'|]
              conn <- liftIO $ PSQL.connectPostgreSQL $ BC.pack connString

              -- Use the below to simulate this API call taking too long to complete
              -- _ <- liftIO $ threadDelay 5000000

              res <- liftIO $ DB.createReminder conn rem
              case res of
                Left err -> throwError err505 { errBody = BLC.pack err }
                Right id -> return id

        getUserIDForEmail :: String -> Handler (Maybe UserID)
        getUserIDForEmail email = do
          eConnInfo <- liftIO $ CI.getConnectionInfo
          case eConnInfo of
            Left err -> throwError err505 { errBody = BLC.pack err }
            Right connInfo -> do
              conn <- liftIO $ connect ConnectInfo {connectHost = host connInfo
                                                   ,connectPort = (fromIntegral $ port connInfo)
                                                   ,connectDatabase = database connInfo
                                                   ,connectUser = user connInfo
                                                   ,connectPassword = password connInfo
                                                   }
              let connString = [i|host='#{host connInfo}' dbname='#{database connInfo}' user='#{user connInfo}' password='#{password connInfo}' port='#{port connInfo}'|]
              conn <- liftIO $ PSQL.connectPostgreSQL $ BC.pack connString
              res <- liftIO $ DB.getUserIDForEmail conn email
              return res

        setTrelloToken :: TrelloToken -> Handler (String, String)
        setTrelloToken token = do
          eConnInfo <- liftIO $ CI.getConnectionInfo
          case eConnInfo of
            Left err -> throwError err505 { errBody = BLC.pack err }
            Right connInfo -> do
              conn <- liftIO $ connect ConnectInfo {connectHost = host connInfo
                                                   ,connectPort = (fromIntegral $ port connInfo)
                                                   ,connectDatabase = database connInfo
                                                   ,connectUser = user connInfo
                                                   ,connectPassword = password connInfo
                                                   }
              let connString = [i|host='#{host connInfo}' dbname='#{database connInfo}' user='#{user connInfo}' password='#{password connInfo}' port='#{port connInfo}'|]
              conn <- liftIO $ PSQL.connectPostgreSQL $ BC.pack connString
              res <- liftIO $ DB.setTrelloToken conn token
              case res of
                Left err -> throwError err505 { errBody = BLC.pack err }
                Right r -> return r

        getTrelloToken :: String -> Handler String
        getTrelloToken trelloID = do
          eConnInfo <- liftIO $ CI.getConnectionInfo
          case eConnInfo of
            Left err -> throwError err505 { errBody = BLC.pack err }
            Right connInfo -> do
              conn <- liftIO $ connect ConnectInfo {connectHost = host connInfo
                                                   ,connectPort = (fromIntegral $ port connInfo)
                                                   ,connectDatabase = database connInfo
                                                   ,connectUser = user connInfo
                                                   ,connectPassword = password connInfo
                                                   }
              let connString = [i|host='#{host connInfo}' dbname='#{database connInfo}' user='#{user connInfo}' password='#{password connInfo}' port='#{port connInfo}'|]
              conn <- liftIO $ PSQL.connectPostgreSQL $ BC.pack connString
              res <- liftIO $ DB.getTrelloToken conn trelloID
              case res of
                Nothing -> throwError err505 { errBody = BLC.pack $ "No token found for trello id: " <> trelloID }
                Just tok -> return tok

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
