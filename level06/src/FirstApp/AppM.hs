{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FirstApp.AppM where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           FirstApp.Types         (Conf, FirstAppDB)

-- First, let's clean up our (Conf,FirstAppDB) with an application Env type. We
-- will add a general purpose logging function as well. Remember that functions
-- are values, we're able to pass them around and place them on records like any
-- other type.
data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- It would be nice to remove the need to pass around our Env to every function
-- that needs it. Wouldn't it be great to have our functions run where we could
-- simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:

newtype AppM a = AppM ( Env -> IO a )

-- This gives us a type that declares this function has access to our Env, and
-- will do something involving IO. It's another form of documentation and type
-- safety. AppM only has one definition and so we can easily understand what it
-- implies when used in our application.
--
-- This structure allows us to start writing our functions in terms of
-- constraints. As an example, if we wanted to abstract over IO and indicate
-- that instead of the concrete type we wanted a constraint that allows for IO
-- actions. Our AppM would look more like this:
--
-- AppM m a = AppM ( Env -> m a )
--
-- Then our functions would look like:
--
-- foo :: MonadIO m => Int -> AppM m a
--
-- Or we could not use a concrete type for Env
--
-- AppM e m a = AppM ( e -> m a )
--

runAppM    -- if we give this an AppM a, then it returns a function from Env -> IO a.
  :: AppM a
  -> Env
  -> IO a
runAppM (AppM f) env =
  f env
-- or even:   runAppM (AppM f) = f

-- So we can use AppM in place of IO, we need to define the stuff below.

-- Recall: newtype AppM a = AppM ( Env -> IO a )
instance Functor AppM where
  fmap :: (a -> b) -> AppM a -> AppM b
  -- the RHS of the \e -> needs to be :: IO b
  fmap f mofA = AppM (\e -> fmap f $ runAppM mofA e)   -- fmap is working on IO here.

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM (\_ -> pure a)   -- RHS of AppM needs to be :: Env -> IO a

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  (<*>) mf mofA = AppM (\e -> runAppM mf e <*> runAppM mofA e)

  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b 
  -- (<*>) :: IO => IO (a -> b) -> IO a -> IO b

instance Monad AppM where
  return :: a -> AppM a
  return = pure
  -- return a = AppM (\_ -> return a)

  -- When it comes to running functions in AppM as a Monad, this will take care
  -- of passing the Env from one function to the next.
  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  (>>=) mofA f = AppM (\e -> runAppM mofA e >>= (\a -> runAppM (f a) e))

  -- (>>=) mofA f = AppM (\e -> do
  --   a <- runAppM mofA e
  --   runAppM (f a) e)

  -- (>>=) :: IO a -> (a -> IO b) -> IO b

instance MonadReader Env AppM where
  -- Return the current Env from the AppM.
  ask :: AppM Env
  ask = AppM (\e -> pure e)
  -- newtype AppM Env = AppM ( Env -> IO Env ) 

  -- Run an AppM inside of the current one using a modified Env value.
  -- eg. to change logging level in a deep part of the program.
  local :: (Env -> Env) -> AppM a -> AppM a
  local f mofA = AppM (\e -> runAppM mofA (f e))

  -- This will run a function on the current Env and return the result.
  reader :: (Env -> a) -> AppM a
  reader f = AppM (\e -> pure $ f e)

instance MonadIO AppM where
  -- Take a type of 'IO a' and lift it into our AppM.
  liftIO :: IO a -> AppM a
  liftIO ioa = AppM (\e -> ioa)

-- Move on to ``src/FirstApp/DB.hs`` after this
