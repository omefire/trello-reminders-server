{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.ByteString as BS
import qualified Text.Email.Validate as Email
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import ConnectionInfo

type EmailAPI = "getEmailsForUser" :> Capture "email" Email :> Get '[JSON] [Email]

newtype Email = Email String deriving (Eq, Show, Generic)

instance ToJSON Email

instance FromHttpApiData Email where
  -- parseUrlPiece :: Text -> Either Text Email
  -- TODO: What happens when the email provided is invalid?
  parseUrlPiece str | Email.isValid $ BC.pack (T.unpack str) = Right $ Email $ T.unpack str
                    | otherwise         = Left $ T.pack "Invalid email address provided"

-- type Email = String
-- instance ToJSON Email
-- https://stackoverflow.com/questions/5941701/why-can-i-not-make-string-an-instance-of-a-typeclass

emails1 :: [Email]
emails1 =
  [
    Email "omef@gmail.com",
    Email "imef@gmail.com",
    Email "hamef@gmail.com"
  ]

-- TODO: What if the input email is a valid email but doesn't exist in the database?
-- TODO: Security: How to prevent people from getting other accounts' emails by spoofing their account's email?
server1 :: Server EmailAPI
server1 = emails
  where emails :: Email -> Handler [Email]
        emails (Email em) = do
          (connectHost, connectPort, connectDatabase, connectPassword, connectUser) <- liftIO $ getConnectionInfo
          conn <- liftIO $ connect ConnectInfo {connectHost="localhost"
                                               ,connectPort=5432
                                               ,connectDatabase="trello-reminders-db"
                                               ,connectPassword="type_your_password_here"
                                               ,connectUser="postgres"
                                               }
          emls <- liftIO $ DB.getEmailsForUser conn em
          return $ map (\e -> Email e) emls

emailAPI :: Proxy EmailAPI
emailAPI = Proxy

app1 :: Application
app1 = serve emailAPI server1

main :: IO ()
main = run 8081 app1
