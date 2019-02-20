 {-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module DB (getEmailsForUser, createReminder) where

import Opaleye
import Data.Profunctor.Product (p2, p4)
import Database.PostgreSQL.Simple
import Control.Arrow (returnA)
import qualified Types as Types
import qualified Chronos as Chronos
import Data.Time
import Data.Fixed

import Opaleye
import Opaleye.Trans

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Maybe
import qualified Database.PostgreSQL.Simple as PSQL



type Email = String
-- type User = String

-- data User' a b = User { usID :: a, usEmail :: b }
-- type UserField = User' (Field SqlText) (Field SqlText)

emailTable :: Table (Field SqlInt4, Field SqlText) (Field SqlInt4, Field SqlText)
emailTable = table "Emails" (p2 ( tableField "ID"
                                , tableField "Email"))

userTable :: Table (Field SqlInt4, Field SqlText) (Field SqlInt4, Field SqlText)
userTable = table "Users" (p2 ( tableField "ID"
                              , tableField "Email"))

userEmailTable :: Table (Field SqlInt4, Field SqlInt4) (Field SqlInt4, Field SqlInt4)
userEmailTable = table "Users_Emails" (p2 ( tableField "UserID"
                                          , tableField "EmailID"))


-- Reminders
data ReminderP i n d dt = ReminderP
  { remId :: i
  , remName :: n
  , remDescription :: d
  , remDateTime :: dt
  } deriving (Show, Eq)

type WriteReminder = ReminderP (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamptz)
type ReadReminder = ReminderP (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz)

makeAdaptorAndInstance "pReminder" ''ReminderP

reminderTable :: Table (WriteReminder) (ReadReminder)
reminderTable = Table "Reminders" $ pReminder ReminderP
  { remId = optional "ID"
  , remName = required "Name"
  , remDescription = required "Description"
  , remDateTime = required "ReminderDateTime"
  }

insertReminder :: Types.Reminder -> Transaction (Maybe Int)
insertReminder rem = do
  reminderId <- ( insertReturningFirst reminderTable remId
                  ( ReminderP Nothing (pgString $ Types.reminderName rem) (pgString $ Types.reminderDescription rem) (pgUTCTime $ Types.reminderDateTime rem) ) ) :: Transaction (Maybe Int)
  case reminderId of
    Nothing -> return Nothing
    Just rId -> return (Just rId)

---

userSelect :: Select (Field SqlInt4, Field SqlText)
userSelect = selectTable userTable

emailSelect :: Select (Field SqlInt4, Field SqlText)
emailSelect = selectTable emailTable

userEmailSelect :: Select (Field SqlInt4, Field SqlInt4)
userEmailSelect = selectTable userEmailTable

userAndEmails :: SelectArr (Field SqlText) (Field SqlText)
userAndEmails = proc email -> do
  (users_userID, users_userEmail)   <- userSelect -< ()
  restrict  -< (email .== users_userEmail)

  (usersEmails_userID, usersEmails_emailID) <- userEmailSelect -< ()
  restrict -< (usersEmails_userID .== users_userID)

  (emails_emailID, emails_email)  <- emailSelect -< ()
  restrict -< (emails_emailID .== usersEmails_emailID)

  returnA -< (emails_email)


-- Top-level functions

getEmailsForUser :: Connection -> Email -> IO [Email]
getEmailsForUser conn email = runQuery conn $ proc () -> do
  (users_userID, users_userEmail)   <- userSelect -< ()
  restrict  -< (sqlString email .== users_userEmail)

  (usersEmails_userID, usersEmails_emailID) <- userEmailSelect -< ()
  restrict -< (usersEmails_userID .== users_userID)

  (emails_emailID, emails_email)  <- emailSelect -< ()
  restrict -< (emails_emailID .== usersEmails_emailID)

  returnA -< (emails_email)

-- TODO: Add an entry into each of the tables: Reminders, Users_Reminders & Reminders_Emails
-- TODO: Make use of DB transactions
createReminder :: Connection -> Types.Reminder -> IO Types.Reminder
createReminder conn rem = do
  let day = fromGregorian 2019 03  05 -- March 5th, 2019
  let diffTime = secondsToDiffTime 100
  conn <- PSQL.connectPostgreSQL "host='localhost' dbname='trello-reminders-db' user='postgres' password='mission19' port=5432"
  rId <- runOpaleyeT conn (transaction (insertReminder rem))
  return $ Types.Reminder{ Types.reminderId = (fromJust rId),
                           Types.reminderName = show (fromJust rId),
                           Types.reminderDescription = "ABC",
                           Types.reminderDateTime = UTCTime { utctDay = day, utctDayTime = diffTime }, -- 2014 2 26 17 58 52,
                           Types.reminderEmails = [Types.Email "omefire@gmail.com", Types.Email "imefire@gmail.com"]
                         }
