{-# LANGUAGE GADTs, FlexibleInstances #-}
module App
  ( runApp
  , runMockApp
  , App
  , MonadLog
  ) where

-- import SViewer.Settings
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

data Env = Env
  { envLog :: !(String -> IO ())
  }

class Monad m => MonadLog m where
  putLog :: String -> m ()

instance MonadIO m => MonadLog (ReaderT Env m) where
  putLog s = do
    env <- ask
    liftIO $ envLog env $ s

type App = ReaderT Env IO

normalEnv = Env
  { envLog = putStrLn }

mockEnv = Env
  { envLog = putStrLn }

runApp :: App a -> IO a
runApp program = runReaderT program normalEnv

runMockApp :: App a -> IO a
runMockApp program = runReaderT program mockEnv
