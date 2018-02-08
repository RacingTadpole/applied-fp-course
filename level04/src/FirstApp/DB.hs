{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module FirstApp.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)
import           Data.Bifunctor                     (first)

import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.Types                     (Comment, CommentText,
                                                     Error(..), Topic, getTopic, mkTopic,
                                                     fromDbComment, getCommentText)
import           FirstApp.DB.Types                  (DBComment)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB db =
  Sql.close $ dbConn db

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction makeDBGoNow
  where
    makeDBGoNow = do
      conn <- Sql.open fp
      Sql.execute_ conn createTableQ
      pure $ FirstAppDB conn
      where
      -- Query has an `IsString` instance so string literals like this can be
      -- converted into a `Query` type when the `OverloadedStrings` language
      -- extension is enabled.
        createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DbComment to a Comment, we need to use ``fromDbComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments db t =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    q :: IO [DBComment]  -- shouldn't be needed in finished code.
    q = Sql.query (dbConn db) sql (Sql.Only $ getTopic t)
  -- There are several possible implementations of this function. Paritcularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.
  in do
    eitherRRs <- Sql.runDBAction q
    -- eitherRRs :: Either SQLiteResponse [DBComment]
    -- First, convert these to :: Either Error [DBComment]
    let eitherERs = first SQLError eitherRRs
    -- We have:
    -- traverse :: (Applicative f, Traversable t) => (a -> fb) -> t a -> f (t b)
    -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
    let eitherComments = eitherERs >>= traverse fromDbComment
    pure eitherComments

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic db t c =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in do
    now <- getCurrentTime
    let x = Sql.execute (dbConn db) sql (getTopic t, getCommentText c, now)
    -- runDBAction :: IO a -> IO (Either SQLiteResponse a)
    rawResult <- Sql.runDBAction x
    let result = first SQLError rawResult
    pure result


getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics db =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    q = Sql.query_ (dbConn db) sql
    -- need to pull out the text from within the Sql.Only containers.
    -- q :: IO [Sql.Only Text]
  in do
    eitherRTs <- Sql.runDBAction q
    let eitherETs = first SQLError eitherRTs
    -- We have:
    -- traverse :: (Applicative f, Traversable t) => (a -> fb) -> t a -> f (t b)
    -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
    let eitherTopics = eitherETs >>= traverse (mkTopic . Sql.fromOnly)
    pure eitherTopics

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic db t =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    x = Sql.execute (dbConn db) sql (Sql.Only . getTopic $ t)
  in do
    -- runDBAction :: IO a -> IO (Either SQLiteResponse a)
    rawResult <- Sql.runDBAction x
    let result = first SQLError rawResult
    pure result
