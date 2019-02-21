{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module DB (getEmailsForUser, createReminder) where

import Opaleye
import Data.Profunctor.Product (p2, p4)
-- import Database.PostgreSQL.Simple
import Control.Arrow (returnA)
import Types
import qualified Chronos as Chronos
import Data.Time
import Data.Fixed

import Opaleye
import Opaleye.Trans

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Maybe
import qualified Database.PostgreSQL.Simple as PSQL
import Control.Monad.Trans.Maybe


-- type Email = String
-- type User = String

-- data User' a b = User { usID :: a, usEmail :: b }
-- type UserField = User' (Field SqlText) (Field SqlText)

emailTable' :: Table (Field SqlInt4, Field SqlText) (Field SqlInt4, Field SqlText)
emailTable' = table "Emails" (p2 ( tableField "ID"
                                , tableField "Email"))

userTable' :: Table (Field SqlInt4, Field SqlText) (Field SqlInt4, Field SqlText)
userTable' = table "Users" (p2 ( tableField "ID"
                              , tableField "Email"))

userEmailTable' :: Table (Field SqlInt4, Field SqlInt4) (Field SqlInt4, Field SqlInt4)
userEmailTable' = table "Users_Emails" (p2 ( tableField "UserID"
                                          , tableField "EmailID"))


-- Tables
data ReminderP i n d dt = ReminderP
  { remID :: i
  , remName :: n
  , remDescription :: d
  , remDateTime :: dt
  } deriving (Show, Eq)

data UserReminderP a b = UserReminderP
  { urUserID :: a
  , urReminderID :: b
  } deriving (Show, Eq)

data UserEmailP a b = UserEmailP
  { ueUserID :: a
  , ueEmailID :: b
  }

data ReminderEmailP a b = ReminderEmailP
  { reReminderID :: a
  , reEmailID :: b
  }

data UserP a b = UserP
  { uuserID :: a
  , uuserEmail :: b
  }

data EmailP a b = EmailP
  { emID :: a
  , emEmail :: b
  }

type WriteReminder = ReminderP (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamptz)
type ReadReminder = ReminderP (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz)

type WriteUserReminder = UserReminderP (Column PGInt4) (Column PGInt4)
type ReadUserReminder = UserReminderP (Column PGInt4) (Column PGInt4)

type WriteUserEmail = UserEmailP (Column PGInt4) (Column PGInt4)
type ReadUserEmail = UserEmailP (Column PGInt4) (Column PGInt4)

type WriteReminderEmail = ReminderEmailP (Column PGInt4) (Column PGInt4)
type ReadReminderEmail = ReminderEmailP (Column PGInt4) (Column PGInt4)

type WriteUser = UserP (Maybe (Column PGInt4)) (Column PGText)
type ReadUser = UserP (Column PGInt4) (Column PGText)

type WriteEmail = EmailP (Maybe (Column PGInt4)) (Column PGText)
type ReadEmail = EmailP (Column PGInt4) (Column PGText)

makeAdaptorAndInstance "pReminder" ''ReminderP
makeAdaptorAndInstance "pUserReminder" ''UserReminderP
makeAdaptorAndInstance "pUserEmail" ''UserEmailP
makeAdaptorAndInstance "pReminderEmail" ''ReminderEmailP
makeAdaptorAndInstance "pUser" ''UserP
makeAdaptorAndInstance "pEmail" ''EmailP

reminderTable :: Table (WriteReminder) (ReadReminder)
reminderTable = Table "Reminders" $ pReminder ReminderP
  { remID = optional "ID"
  , remName = required "Name"
  , remDescription = required "Description"
  , remDateTime = required "ReminderDateTime"
  }

userReminderTable :: Table (WriteUserReminder) (ReadUserReminder)
userReminderTable = Table "Users_Reminders" $ pUserReminder UserReminderP
  { urUserID = required "UserID"
  , urReminderID = required "ReminderID"
  }

userEmailTable :: Table (WriteUserEmail) (ReadUserEmail)
userEmailTable = Table "Users_Emails" $ pUserEmail UserEmailP
  { ueUserID = required "UserID"
  , ueEmailID = required "EmailID"
  }

reminderEmailTable :: Table (WriteReminderEmail) (ReadReminderEmail)
reminderEmailTable = Table "Reminders_Emails" $ pReminderEmail ReminderEmailP
  { reReminderID = required "ReminderID"
  , reEmailID = required "EmailID"
  }

userTable :: Table (WriteUser) (ReadUser)
userTable = Table "Users" $ pUser UserP
  { uuserID = optional "ID"
  , uuserEmail = required "Email"
  }

emailTable :: Table (WriteEmail) (ReadEmail)
emailTable = Table "Emails" $ pEmail EmailP
  { emID = optional "ID"
  , emEmail = required "Email"
  }

insertReminder :: Types.Reminder -> Transaction (Maybe Int)
insertReminder rem = do
  reminderId <- ( insertReturningFirst reminderTable remID
                  ( ReminderP Nothing (pgString $ Types.reminderName rem) (pgString $ Types.reminderDescription rem) (pgUTCTime $ Types.reminderDateTime rem) ) ) :: Transaction (Maybe Int)
  case reminderId of
    Nothing -> return Nothing
    Just rId -> return (Just rId)

insertUserReminder :: Int -> Int -> Transaction (Maybe (Int, Int))
insertUserReminder userId reminderId = do
  res <- insertReturningFirst userReminderTable (\rem -> (urUserID rem, urReminderID rem))
           ( UserReminderP (pgInt4 userId) (pgInt4 reminderId) )
  case res of
    Nothing -> return Nothing
    Just (uId, rId) -> return $ Just (uId, rId)

insertUserEmail :: Int -> Int -> Transaction (Maybe (Int, Int))
insertUserEmail userId reminderId = do
  res <- insertReturningFirst userEmailTable (\r -> (ueUserID r, ueEmailID r))
           ( UserEmailP (pgInt4 userId) (pgInt4 reminderId) )
  case res of
    Nothing -> return Nothing
    Just (uId, eId) -> return $ Just (uId, eId)

insertReminderEmail :: Int -> Int -> Transaction (Maybe (Int, Int))
insertReminderEmail reminderId emailId = do
  res <- insertReturningFirst reminderEmailTable (\r -> (reReminderID r, reEmailID r))
           ( ReminderEmailP (pgInt4 reminderId) (pgInt4 emailId) )
  case res of
    Nothing -> return Nothing
    Just (remId, emId) -> return $ Just (remId, emId)

---

userSelect :: Select (Field SqlInt4, Field SqlText)
userSelect = selectTable userTable'

emailSelect :: Select (Field SqlInt4, Field SqlText)
emailSelect = selectTable emailTable'

userEmailSelect :: Select (Field SqlInt4, Field SqlInt4)
userEmailSelect = selectTable userEmailTable'

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

selectEmailsForUser :: UserID -> Transaction [ EmailP Int String ]
selectEmailsForUser (UserID uid) = query $ emailsForUserQuery uid
  where
    emailsForUserQuery :: Int -> Query ReadEmail
    emailsForUserQuery uid = proc() -> do
      user <- selectTable userTable -< ()
      restrict -< (uuserID user) .== (pgInt4 uid)

      userEmail <- selectTable userEmailTable -< ()
      restrict -< ( (ueUserID userEmail) .== (uuserID user) )

      email <- selectTable emailTable -< ()
      restrict -< (ueEmailID userEmail) .== (emID email)

      returnA -< email

getEmailsForUser :: PSQL.Connection -> UserID -> IO [Email]
-- getEmailsForUser conn uid = return [Email { emailID = 1, emailValue = "" }]
getEmailsForUser conn userid = do
  emails <- runOpaleyeT conn $ transaction $ selectEmailsForUser userid
  return $ (flip map) emails $ \(EmailP id val) -> Email { emailID = id, emailValue = val }

  -- user <- selectTable userTable -< ()
  -- restrict -< (uuserID user) .== (pgInt4 $ userID userid)

  -- userEmail <- selectTable userEmailTable -< ()
  -- restrict -< ( (ueUserID userEmail) .== (uuserID user) )

  -- email <- selectTable emailTable -< ()
  -- restrict -< (ueEmailID userEmail) .== (emID email)

  -- returnA -< (Email { emailID = (emID email), emailValue = (emEmail email) })

-- Getemailsforuser' :: Connection -> Email -> IO [Email]
-- getEmailsForUser' conn email = runQuery conn $ proc () -> do
--   (users_userID, users_userEmail)   <- userSelect -< ()
--   restrict  -< (sqlString email .== users_userEmail)

--   (usersEmails_userID, usersEmails_emailID) <- userEmailSelect -< ()
--   restrict -< (usersEmails_userID .== users_userID)

--   (emails_emailID, emails_email)  <- emailSelect -< ()
--   restrict -< (emails_emailID .== usersEmails_emailID)

--   returnA -< (emails_email)

-- TODO: Add an entry into each of the tables: Reminders, Users_Reminders & Reminders_Emails
-- TODO: Make use of DB transactions
createReminder :: PSQL.Connection -> Types.Reminder -> IO (Maybe Types.Reminder)
createReminder conn rem = do
  let day = fromGregorian 2019 03  05 -- March 5th, 2019
  let diffTime = secondsToDiffTime 100
  runOpaleyeT conn $ transaction $ runMaybeT $ do
    remId <- MaybeT $ insertReminder rem
    (userId, _) <- MaybeT $ ( ( insertUserReminder (Types.reminderUserID rem) remId ) :: Transaction (Maybe (Int, Int)) )
    _ <- MaybeT $ ( ( insertUserEmail (Types.reminderUserID rem) remId ) :: Transaction (Maybe (Int, Int)) )
    let emails = Types.reminderEmails rem
    --(flip map) emails $\e -> do
    --  insertReminderEmail (Types.reminderId rem) ()

    return $ Types.Reminder{ Types.reminderUserID = 1,
                             Types.reminderID = 1,
                             Types.reminderName = "ABC",
                             Types.reminderDescription = "DESC",
                             Types.reminderDateTime = UTCTime { utctDay = day, utctDayTime = diffTime },
                             Types.reminderEmails = [Types.Email { Types.emailID = 1, Types.emailValue = "omefire@gmail.com" }]
                           }


  -- rId <- runOpaleyeT conn $ transaction $ do
  --   mRemId <- insertReminder rem
  --   case mRemId of
  --     Nothing -> Nothing
  --     Just remId -> do
  --       mUserRemIds <- insertUserReminder (reminderUserID rem) remId
  --       case mUserRemIds of
  --         Nothing -> Nothing
  --         Just userRemIds -> do
  --           return $ Types.Reminder{ Types.reminderId = (fromJust rId),
  --                                    Types.reminderName = show (fromJust rId),
  --                                    Types.reminderDescription = "ABC",
  --                                    Types.reminderDateTime = UTCTime { utctDay = day, utctDayTime = diffTime }, -- 2014 2 26 17 58 52,
  --                                    Types.reminderEmails = [Types.Email "omefire@gmail.com", Types.Email "imefire@gmail.com"]
  --                                  }
