
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Servant.API
import GHC.Generics

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Chronos as Chronos

import qualified Text.Email.Validate as Email

import Data.Time


-- Email
newtype Email = Email String deriving (Eq, Show, Generic)

instance ToJSON Email
instance FromJSON Email

instance FromHttpApiData Email where
  -- parseUrlPiece :: Text -> Either Text Email
  -- TODO: What happens when the email provided is invalid?
  parseUrlPiece str | Email.isValid $ BC.pack (T.unpack str) = Right $ Email $ T.unpack str
                    | otherwise         = Left $ T.pack "Invalid email address provided"


-- Reminder
data Reminder = Reminder { reminderId :: Int,
                           reminderName :: String,
                           reminderDescription :: String,
                           reminderDateTime :: UTCTime,
                           reminderEmails :: [Email]
                         } deriving (Show, Generic)

instance ToJSON Reminder
instance FromJSON Reminder

-- instance ToJSON Chronos.Datetime
-- instance FromJSON Chronos.Datetime
-- instance Generic Chronos.Datetime
