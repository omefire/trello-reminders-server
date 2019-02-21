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


-- http://localhost:8081/getEmailsForUser/omefire@gmail.com
type EmailAPI = "getEmailsForUser" :> Capture "email" Email :> Get '[JSON] [Email]
type ReminderAPI = "createReminder" :> ReqBody '[JSON] Reminder :> Post '[JSON] (Maybe Reminder)

type API = EmailAPI :<|> ReminderAPI

-- TODO: Security: How to prevent people from getting other accounts' emails by spoofing their account's email?
-- TODO: Remove code duplication
server :: Server API
server = getEmailsForUser :<|> createReminder
  where getEmailsForUser :: Email -> Handler [Email]
        getEmailsForUser (Email em) = do
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
              emls <- liftIO $ DB.getEmailsForUser conn em
              return $ map (\e -> Email e) emls

        -- Example JSON body request
        -- {
	--  "reminderName": "ABC",
	--  "reminderDescription": "ABCDESC",
	--  "reminderDate": "2016-12-09T15:04:26.349857693845+05:00",
	--  "reminderEmails": ["omefire@gmail.com"]
        -- }
        --
        -- {
        --  "reminderDate": "2019-03-05T00:01:40Z",
        --  "reminderEmails": [
        --    "omefire@gmail.com",
        --    "imefire@gmail.com"
        --  ],
        --  "reminderName": "ABC",
        --  "reminderDescription": "ABC"
        -- }
        createReminder :: Reminder -> Handler (Maybe Reminder)
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
              reminder <- liftIO $ DB.createReminder conn rem
              return reminder

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
