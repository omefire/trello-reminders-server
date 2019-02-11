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
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as BSLI
import qualified Text.Email.Validate as Email
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import ConnectionInfo as CI
import Control.Monad.Except (throwError)

-- http://localhost:8081/getEmailsForUser/omefire@gmail.com
type EmailAPI = "getEmailsForUser" :> Capture "email" Email :> Get '[JSON] [Email]

newtype Email = Email String deriving (Eq, Show, Generic)

instance ToJSON Email

instance FromHttpApiData Email where
  -- parseUrlPiece :: Text -> Either Text Email
  -- TODO: What happens when the email provided is invalid?
  parseUrlPiece str | Email.isValid $ BC.pack (T.unpack str) = Right $ Email $ T.unpack str
                    | otherwise         = Left $ T.pack "Invalid email address provided"


-- TODO: Security: How to prevent people from getting other accounts' emails by spoofing their account's email?
server :: Server EmailAPI
server = emails
  where emails :: Email -> Handler [Email]
        emails (Email em) = do
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

emailAPI :: Proxy EmailAPI
emailAPI = Proxy

app :: Application
app = serve emailAPI server

main :: IO ()
main = run 8081 app
