{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveGeneric              #-}
module FirstApp.DB.Types where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)
import           GHC.Generics                   (Generic)

import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

-- To try to avoid leaking various types and expected functionality around the
-- application, we create a stand alone type that will represent the data we
-- store in the database. In this instance, it is the raw types that make up a
-- comment.

-- Complete in the DbComment type below so it is a record type that matches the
-- Comment type, but without the newtype wrappers for each value. To get started,
-- just copy the new definition for the `Comment` type from FirstApp.Types.
data DBComment = DBComment
  -- NB: Haskell does not allow duplicate field names for records so the field
  -- names for this type will have to be slightly different
  { dbCommentId    :: Int
  , dbCommentTopic :: Text
  , dbCommentBody  :: Text
  , dbCommentTime  :: UTCTime
  }
  deriving ( Show, Generic )

-- This Typeclass comes from the `sqlite-simple` package and describes how to
-- decode a single row from the database into a single representation of our
-- type. This technique of translating a result row to a type will differ
-- between different packages/databases.
instance FromRow DBComment where
    fromRow = DBComment <$> field <*> field <*> field <*> field
    -- or:
    -- fromRow = pure DBComment   -- RowParser (Int -> Text -> ... -> DBComment)
    --   <*> field                -- RowParser (Int)
    --   <*> field ... etc

-- Now move to ``src/FirstApp/Types.hs``
