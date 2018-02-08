{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module FirstApp.AppM where

import           Control.Applicative    (liftA2)
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           FirstApp.DB.Types      (FirstAppDB)
import           FirstApp.Types         (Conf, Error)

import           Data.Bifunctor         (first)

data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will expand the capabilities of our AppM by including the
-- Either type in our definition. We will also rework our Monad instance to stop
-- processing when it encounters a Left value.
--
-- This will work in the same manner as the Functor/Applicative/Monad
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value.
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     alsoMightFail :: Int -> IO (Either Error Value)
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.

newtype AppM a = AppM (Env -> IO (Either Error a))
  deriving Functor

-- The runAppM function only needs to change the final return type as it has an
-- 'Either Error' and not just the 'a'.
runAppM
  :: AppM a
  -> Env
  -> IO (Either Error a)
runAppM (AppM m) =
  m

-- Copy over your previously completed definitions.

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM (\_ -> pure . pure $ a)

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  -- need :: Env -> IO (Either Error b)
  -- runAppM mf e :: IO (Either Error (a->b))
  -- runAppM mofA e :: IO (Either Error a)
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- runAppM mf e <*> runAppM mofA e :: IO (Either Error b)

  -- First go (works):
  -- (<*>) mf mofA = AppM (\e -> (x e) <*> runAppM mofA e)
  --   where
  --     x e = do
  --       f <- runAppM mf e  -- needs to be x :: IO (Either Error a -> Either Error b)
  --       -- we have runAppM mf e :: IO (Either Error (a->b))
  --       pure (f <*>)
  -- -- but check out :t fmap (<*>).

  -- Second go:
  -- (<*>) mf mofA = AppM (\e -> (x e) <*> runAppM mofA e)
  --   where
  --     x e = fmap (<*>) (runAppM mf e)  

  -- As suggested by Sean:
--  (<*>) mf mofA = AppM (\e -> (<*>) <$> runAppM mf e <*> runAppM mofA e)

  -- Can also do with liftA2 but this isn't tested:
  (<*>) mf mofA = AppM (\e -> liftA2 (<*>) (runAppM mf e) (runAppM mofA e))


instance Monad AppM where
  return :: a -> AppM a
  return = pure

  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  -- need :: Env -> IO (Either Error b)
  -- runAppM (f a) e:: IO (Either Error b)
  -- runAppM mofA e :: IO (Either Error a)
  -- (>>=) :: f a -> (a -> f b) -> f b

  -- From before:
  -- (>>=) mofA f = AppM (\e -> runAppM mofA e >>= (\a -> runAppM (f a) e))

  (>>=) mofA f = AppM (\e -> do
      z <- runAppM mofA e   -- z :: Either Error a
      -- need to finish do with :: IO (Either Error b)
      -- runAppM :: AppM b -> Env -> IO (Either Error b)
      -- So we just need an AppM b.
      -- We can get one from a using f :: a -> AppM b.
      -- We want to run this through runAppM (f _z) e, but in this we need _z :: a
      case z of
        Right r -> runAppM (f r) e
        Left err -> pure $ Left err    -- _f err, _f :: Error -> IO (Either Error b)
    )

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  -- liftIO ioa = AppM (\_ -> do
  --     eea <- ioa
  --     pure $ pure eea   -- or, pure $ Right eea
  --   )
  -- OR, neater:
  liftIO ioa = AppM (\_ -> fmap pure ioa)


instance MonadReader Env AppM where
  ask :: AppM Env
  ask = AppM (\e -> pure . pure $ e)
  -- newtype AppM Env = AppM ( Env -> IO (Either Error Env) )

  local :: (Env -> Env) -> AppM a -> AppM a
  -- runAppM :: AppM a -> Env -> IO (Either Error a)
  -- Wow, no change from level06.
  local f mofA = AppM (\e -> runAppM mofA (f e))

  reader :: (Env -> a) -> AppM a
  reader f = AppM (\e -> (pure . pure) $ f e)


instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError err = AppM (\_ -> pure $ Left err)

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError mofA ef = AppM (\e -> do
      z <- runAppM mofA e
      case z of
        Right r -> pure $ Right r
        Left err -> runAppM (ef err) e
    )
    -- do
    -- a' <- mofA
    -- case a' of
    --   Right r -> pure $ Right r
    --   Left err -> ef err

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither
  :: Either Error a
  -> AppM a
liftEither eea =
  case eea of
    Right a' -> pure a'
    Left e' -> throwError e'
  
--- For level 6, we could have used:
--
--   newtype AppM' a = AppM'
--     { unAppM' :: ReaderT Env IO a }
--     deriving ( Functor, Applicative, Monad, MonadIO, MonadReader Env )
--   runAppM' :: AppM' a -> Env -> IO (Either Error a)
--   runAppM' appm e = runExceptT $ runReaderT (unAppM' appm) e

--- For level 7, we can use:
--
--   newtype AppM' a = AppM'
--     { unAppM' :: ReaderT Env (ExceptT Error IO) a }
--     deriving ( Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError Error )
