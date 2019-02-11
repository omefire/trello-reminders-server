{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module DB (getEmailsForUser) where

import Opaleye
import Data.Profunctor.Product (p2)
import Database.PostgreSQL.Simple
import Control.Arrow (returnA)

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

getEmailsForUser :: Connection -> Email -> IO [Email]
getEmailsForUser conn email = runQuery conn $ proc () -> do
  (users_userID, users_userEmail)   <- userSelect -< ()
  restrict  -< (sqlString email .== users_userEmail)

  (usersEmails_userID, usersEmails_emailID) <- userEmailSelect -< ()
  restrict -< (usersEmails_userID .== users_userID)

  (emails_emailID, emails_email)  <- emailSelect -< ()
  restrict -< (emails_emailID .== usersEmails_emailID)

  returnA -< (emails_email)