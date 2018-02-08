{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where
-- module FirstApp.Main where  -- eg. for dev, export everything.

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody, ResponseReceived)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS
import           Data.ByteString.Lazy.Char8 (pack)

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types           (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response

mkResponse s c =
  responseLBS s [("Content-Type", renderContentType c)]

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

-- These next few functions will take raw request information and construct one
-- of our types.
-- Pass the topic and the comment.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t c =
  -- (<*>) :: Either e (a -> b) -> Either e a -> Either e b
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  -- fmap AddRq :: Functor f => f Topic -> f (CommentText -> RqType)
  -- fmap AddRq (mkTopic "foo") :: Either Error (CommentText -> RqType)

  -- want to return Either Error RqType.
  -- mkTopic:: Text -> Either Error Topic

  -- This works:
  -- (<*>) (fmap AddRq (mkTopic t)) (mkCommentText (decodeUtf8 (LBS.toStrict c)))
  
    AddRq   -- or, use pure AddRq and then <*> on the next line instead of <$>.
    <$> mkTopic t   -- <$> is fmap.
    <*> mkCommentText (lazyBytesToStrictText c)
    -- can string more together with <*>, if they do not depend on previous results.
    -- (in fact this lets compiler know it can do the calcs in parallel.
    --  although if mkTopic gives a Left, it doesn't need to do mkCommentText.)
    where
      lazyBytesToStrictText = decodeUtf8 . LBS.toStrict

-- or do it using Monads like this (this is really using >>=).
-- this would allow you to use t' in the calculation of c'.
mkAddRequest'
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest' t c = do
  t' <- mkTopic t 
  c' <- mkCommentText (lazyBytesToStrictText c)
  return $ AddRq t' c'
    where
      lazyBytesToStrictText = decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t =
    ViewRq
    <$> mkTopic t

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

getErrorStatus :: Error -> Status
getErrorStatus BadRoute = status404
getErrorStatus _ = status400   -- for now, default to 400 if not otherwise specified.

getErrorString :: Error -> String
getErrorString EmptyTopic = "Topic cannot be empty"
getErrorString EmptyComment = "Comment cannot be empty"
getErrorString BadRoute = "Not found"

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse e =
  mkResponse (getErrorStatus e) PlainText (pack . getErrorString $ e)

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r =
  -- Equivalent to using do.
  strictRequestBody r
  >>= \b -> 
    return $ case (requestMethod r, pathInfo r) of
      ("GET", ["list"])     -> mkListRequest
      ("GET", [t, "view"])  -> mkViewRequest t
      ("POST", [t, "add"])  -> mkAddRequest t b
      _                     -> Left BadRoute

-- or:
mkRequest'
  :: Request
  -> IO ( Either Error RqType )
mkRequest' r =
  f <$> strictRequestBody r
  where
    f b = case (requestMethod r, pathInfo r) of
        ("GET", ["list"])     -> mkListRequest
        ("GET", [t, "view"])  -> mkViewRequest t
        ("POST", [t, "add"])  -> mkAddRequest t b
        _                     -> Left BadRoute

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest ListRq = Right $ resp200 PlainText "Lists not implemented yet"
handleRequest (ViewRq _) = Right $ resp200 PlainText "View not implemented yet"
handleRequest (AddRq _ _) = Right $ resp200 PlainText "Add not implemented yet"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
  -- equivalently,
  -- :: Application

  -- respond: Response -> IO ResponseReceived
  -- mkRequest :: Request -> IO ( Either Error RqType )
  -- handleRequest :: RqType -> Either Error Response
  -- <$> :: (a -> b) -> f a -> f b

  -- I have f a
  -- I have   a -> f b
  -- (>>=) :: f a -> (a -> f b) -> f b
  -- I want f a -> b
  -- also, either :: (a -> c) -> (b -> c) -> Either a b -> c

app rq respond = do
  x <- mkRequest rq
  respond (g $ x >>= handleRequest)
  where g = either mkErrorResponse id

app' :: Application

app' rq respond =
  mkRequest rq >>= \x -> 
    respond (g $ x >>= handleRequest)
  where g = either mkErrorResponse id



runApp :: IO ()
runApp = run 3000 app
