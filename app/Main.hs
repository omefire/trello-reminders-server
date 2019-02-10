{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
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

server1 :: Server EmailAPI
server1 = emails
  where emails :: Email -> Handler [Email]
        emails (Email em) = return emails1 -- $ map (\(Email e) -> Email (em ++ e)) emails1

emailAPI :: Proxy EmailAPI
emailAPI = Proxy

app1 :: Application
app1 = serve emailAPI server1

main :: IO ()
main = run 8081 app1
