{-# LANGUAGE OverloadedStrings #-}
module FirstApp.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)

import Data.Bifunctor (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow, ToRow,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import FirstApp.AppM (AppM, Env (envDB))

import           FirstApp.Types                     (FirstAppDB (FirstAppDB, dbConn), Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError), Topic,
                                                     fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

-- WOW!
getDBConn
  :: AppM Connection
getDBConn =
  (dbConn . envDB) <$> ask  -- can write this in do or bind notation too

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> AppM (Either Error b)
runDB f g = do
  c <- getDBConn
  r <- liftIO $ Sql.runDBAction $ g c -- Sql.runDBAction :: IO a -> IO (Sql.DatabaseResponse a)
  pure $ either (Left . DBError) f r

  -- Previously:
  -- do
  --   r <- Sql.runDBAction a
  --   pure $ either (Left . DBError) f r
  -- OR:
  -- fmap ( either (Left . DBError) f ) . Sql.runDBAction

getComments
  :: Topic
  -> AppM (Either Error [Comment])
getComments t = do
  -- Write the query with an icky string and remember your placeholders!
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- To be doubly and triply sure we've no garbage in our response, we take care
  -- to convert our DB storage type into something we're going to share with the
  -- outside world. Checking again for things like empty Topic or CommentText values.
  runDB ( traverse fromDbComment ) $ \x -> Sql.query x q [ getTopic t ]

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM (Either Error ())
addCommentToTopic t c = do
  -- Record the time this comment was created.
  now <- liftIO getCurrentTime
  -- Note the triple, matching the number of values we're trying to insert, plus
  -- one for the table name.
  let q =
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  runDB Right $ \x -> Sql.execute x q (getTopic t, getCommentText c, now)
  -- An alternative is to write a returning query to get the Id of the DbComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.

getTopics
  :: AppM (Either Error [Topic])
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) $ \x -> Sql.query_ x q

deleteTopic
  :: Topic
  -> AppM (Either Error ())
deleteTopic t =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB Right $ \x -> Sql.execute x q [getTopic t]
