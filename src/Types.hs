
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
data Email = Email { emailID :: Int
                   , emailValue :: String
                   } deriving (Eq, Show, Generic)

instance ToJSON Email
instance FromJSON Email

-- Reminder
data Reminder = Reminder { reminderID :: Int,
                           reminderName :: String,
                           reminderDescription :: String,
                           reminderDateTime :: UTCTime,
                           reminderEmails :: [Email],
                           reminderUserID :: Int
                         } deriving (Show, Generic)

instance ToJSON Reminder
instance FromJSON Reminder

-- User
newtype UserID = UserID { userID :: Int } deriving (Eq, Show, Generic)

instance FromHttpApiData UserID where
  -- parseUrlPiece :: Text -> Either Text UserID
  -- TODO: What happens if the user supplies something like 'abc' that can't be converted to an integer?
  parseUrlPiece str = do
    let _id = (read (T.unpack str)) :: Int
    return $ UserID _id

instance ToJSON UserID
instance FromJSON UserID

-- TrelloToken
data TrelloToken = TrelloToken { trelloID :: String,
                                 trelloToken :: String
                               } deriving (Show, Generic)
instance ToJSON TrelloToken
instance FromJSON TrelloToken
