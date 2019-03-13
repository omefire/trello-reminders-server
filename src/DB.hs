{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module DB (getEmailsForUser, createReminder, getUserIDForEmail, setTrelloToken, getTrelloToken) where

import Opaleye
import Control.Arrow (returnA)
import Types
import Data.Time
import Opaleye.Trans
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PSQL
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class


-- DB Models

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

data TokenP a b = TokenP
  { tokTrelloID :: a
  , tokToken :: b
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

type WriteToken = TokenP (Column PGText) (Column PGText)
type ReadToken = TokenP (Column PGText) (Column PGText)

makeAdaptorAndInstance "pReminder" ''ReminderP
makeAdaptorAndInstance "pUserReminder" ''UserReminderP
makeAdaptorAndInstance "pUserEmail" ''UserEmailP
makeAdaptorAndInstance "pReminderEmail" ''ReminderEmailP
makeAdaptorAndInstance "pUser" ''UserP
makeAdaptorAndInstance "pEmail" ''EmailP
makeAdaptorAndInstance "pToken" ''TokenP

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

tokenTable :: Table (WriteToken) (ReadToken)
tokenTable = Table "Tokens" $ pToken TokenP
  { tokTrelloID = required "TrelloID"
  , tokToken = required "Token"
  }

insertReminder :: Reminder -> Transaction (Maybe Int)
insertReminder r = do
  reminderId <- ( insertReturningFirst reminderTable remID
                  ( ReminderP Nothing (pgString $ Types.reminderName r) (pgString $ Types.reminderDescription r) (pgUTCTime $ Types.reminderDateTime r) ) ) :: Transaction (Maybe Int)
  case reminderId of
    Nothing -> return Nothing
    Just rId -> return (Just rId)

insertUserReminder :: Int -> Int -> Transaction (Maybe (Int, Int))
insertUserReminder userId reminderId = do
  res <- insertReturningFirst userReminderTable (\r -> (urUserID r, urReminderID r))
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

insertToken :: String -> String -> Transaction (Maybe (String, String))
insertToken trelloId token = do
  res <- insertReturningFirst tokenTable (\r -> (tokTrelloID r, tokToken r))
           ( TokenP (pgString trelloId) (pgString token) )
  return res

-- Top-level functions

getEmailsForUser :: PSQL.Connection -> UserID -> IO [Email]
getEmailsForUser conn userid = do
  emails <- runOpaleyeT conn $ transaction $ selectEmailsForUser userid
  return $ (flip map) emails $ \(EmailP _id val) -> Email { emailID = _id, emailValue = val }
    where
      selectEmailsForUser :: UserID -> Transaction [ EmailP Int String ]
      selectEmailsForUser (UserID uid) = query $ emailsForUserQuery uid

      emailsForUserQuery :: Int -> Query ReadEmail
      emailsForUserQuery uid = proc() -> do
        user <- selectTable userTable -< ()
        restrict -< (uuserID user) .== (pgInt4 uid)

        userEmail <- selectTable userEmailTable -< ()
        restrict -< ( (ueUserID userEmail) .== (uuserID user) )

        email <- selectTable emailTable -< ()
        restrict -< (ueEmailID userEmail) .== (emID email)

        returnA -< email

createReminder :: PSQL.Connection -> Reminder -> IO ( Either String Int )
createReminder conn _rem = do
  res <- runOpaleyeT conn $ transaction $ runMaybeT $ do
    remId <- MaybeT $ insertReminder _rem
    _ <- MaybeT $ ( ( insertUserReminder (reminderUserID _rem) remId ) :: Transaction (Maybe (Int, Int)) )
    let emails = reminderEmails _rem
    _ <- lift $ (flip mapM) emails $ \(Email _id _) -> do
                  insertReminderEmail remId _id
    -- return $ "Successfully created the reminder"
    return remId

  case res of
    Nothing -> return $ Left "An error occured while creating the reminder"
    Just remId -> return $ Right remId


getUserIDForEmail :: PSQL.Connection -> String -> IO (Maybe UserID)
getUserIDForEmail conn email = do
  res <- runOpaleyeT conn $ transaction $ selectUserIDForEmail
  case res of
    Nothing -> return Nothing
    Just user -> return $ Just $ UserID (uuserID user)
  where
    selectUserIDForEmail :: Transaction (Maybe (UserP Int String))
    selectUserIDForEmail = queryFirst $ userIDForEmailQuery

    userIDForEmailQuery :: Query ReadUser
    userIDForEmailQuery  = proc() -> do
      user <- selectTable userTable -< ()
      restrict -< (uuserEmail user) .== (pgString email)
      returnA -< (user)

setTrelloToken :: PSQL.Connection -> TrelloToken -> IO (Either String (String, String))
setTrelloToken conn token = do
  res <- runOpaleyeT conn $ transaction $ insertToken (trelloID token) (trelloToken token)
  case res of
    Nothing -> return $ Left "An error occured while inserting the token"
    Just r -> return $ Right r


getTrelloToken :: PSQL.Connection -> String -> IO (Maybe String)
getTrelloToken conn trelloID = do
  res <- runOpaleyeT conn $ transaction $ selectTrelloToken
  case res of
    Nothing -> return Nothing
    Just tok -> return $ Just $ (tokToken tok)
  where
    selectTrelloToken :: Transaction (Maybe (TokenP String String))
    selectTrelloToken = queryFirst $ trelloTokenQuery

    trelloTokenQuery :: Query ReadToken
    trelloTokenQuery = proc() -> do
      token <- selectTable tokenTable -< ()
      restrict -< (pgString trelloID) .== ((tokTrelloID token))
      returnA -< (token)
