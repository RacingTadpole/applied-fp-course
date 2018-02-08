{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main
  ( runApp
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import           Control.Monad                      (join)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod, responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import           Data.Bifunctor                     (first)
import qualified Data.ByteString.Lazy.Char8         as LBS

import           Data.Either                        (Either (Left, Right),
                                                     either)

import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)

import           Data.Aeson                         (ToJSON)
import qualified Data.Aeson                         as A

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.Conf                      (Conf, firstAppConfig)
import qualified FirstApp.DB                        as DB
import           FirstApp.Types                     (ContentType (JSON, PlainText),
                                                     Error (..),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     mkCommentText, mkTopic,
                                                     renderContentType)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DbInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  x <- prepareAppReqs
  case x of
    Left e -> error (show e)
    Right db -> run 3000 (app db)


-- DB.FirstAppDB
-- IO ( Either StartUpError DB.FirstAppDB )



-- We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
-- 3) Combine the results into a tuple
--
-- The filename for our application config is: "appconfig.json"
--
prepareAppReqs
  :: IO ( Either StartUpError DB.FirstAppDB )
prepareAppReqs = do
  erdb <- DB.initDB "./appconfig.json"    -- :: IO ( Either SQLiteResponse FirstAppDB )
  let r = first DbInitErr erdb
  pure r

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct =
  responseLBS sts [(hContentType, renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- Some new helpers for different statuses and content types
resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 =
  mkResponse status500

resp200Json
  :: ToJSON a
  => a
  -> Response
resp200Json =
  mkResponse status200 JSON . A.encode

-- |
app
  :: DB.FirstAppDB -- ^ Add the Database record to our app so we can use it
  -> Application
app db rq cb = do
  rq' <- mkRequest rq
  resp <- handleRespErr <$> handleRErr rq'
  cb resp
  where
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id

    -- We want to pass the Database through to the handleRequest so it's
    -- available to all of our handlers.
    handleRErr :: Either Error RqType -> IO (Either Error Response)
    handleRErr = either ( pure . Left ) ( handleRequest db )

handleRequest
  :: DB.FirstAppDB
  -> RqType
  -> IO (Either Error Response)
handleRequest db (AddRq t c) =
  -- (<$) :: a -> f b -> f a
  -- (resp200 PlainText "Success" <$) :: (Response <$)
  --                                  :: (f b -> f Response)
  -- fmap or (<$>) :: (c -> d) -> f' c -> f' d
  -- (resp200 PlainText "Success" <$) <$> :: f' f b -> f' f Response
  -- error :: [Char] -> a

  -- addCommentToTopic db t c :: IO (Either Error ())  = f' f b
  (resp200 PlainText "Success" <$) <$> DB.addCommentToTopic db t c

-- handleRequest db (ViewRq t)  =
--   -- Need to output :: IO (Either Error Response)
--   -- eg. dummy answer is: Right $ resp200 PlainText "View Request not implemented"
--   -- getComments :: FirstAppDB -> Topic -> IO (Either Error [Comment])
--   -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
--   -- I want :: IO (Either Error [Comment]) -> IO (Either Error Response)
--   do
--     -- eecs :: Either Error [Comment]
--     eecs <- DB.getComments db t
--     -- Now I want :: Either Error Response
--     -- We have (<$>) :: Functor f => (a -> b) -> f a -> f b
--     let eer = resp200Json <$> eecs
--     -- _x :: FirstApp.Types.Comment -> Either Error b
--     pure eer

handleRequest db (ViewRq t)  =
  (DB.getComments db t) >>= \a ->
  pure (resp200Json <$> a)

  -- (DB.getComments db t) >>=    -- RHS of >>= :: Either Error [Comment] -> IO (Either Error Response)
  --   pure _x
      -- where
      --  x = _z

handleRequest db ListRq =
  -- Need to output :: IO (Either Error Response)
  -- eg. dummy answer is: Right $ resp200 PlainText "List Request not implemented"
  -- getTopics :: FirstAppDB -> IO (Either Error [Topic])
  (fmap . fmap) resp200Json (DB.getTopics db)

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      -> pure ( Left UnknownRoute )

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"
mkErrorResponse (SQLError x) =
  resp400 PlainText (LBS.pack . show $ x)
