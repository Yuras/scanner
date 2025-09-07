-- | Portable drop-in replacement of hspec that works both with GHC and MicroHs
module Test.Hspec
( Spec
, hspec
, describe
, it
, shouldBe
, shouldThrow
, shouldSatisfy
, context
) where

import Data.IORef
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class

type Spec = IO ()

hspec :: Monad m => m a -> m a
hspec m = m

context :: MonadIO m => String -> m a -> m a
context name m = do
  liftIO $ putStrLn "............."
  liftIO $ putStrLn name
  m

describe :: MonadIO m => String -> m a -> m a
describe name m = do
  liftIO $ putStrLn "-------------"
  liftIO $ putStrLn name
  m

it :: MonadIO m => String -> m a -> m a
it name m = do
  liftIO $ putStrLn name
  m

shouldBe :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe a b = unless (a == b) $ do
  liftIO $ putStrLn $ "FAILED: expected " ++ show b ++ ", but got " ++ show a

shouldSatisfy :: (MonadIO m, Show a) => a -> (a -> Bool) -> m ()
shouldSatisfy a f = unless (f a) $ do
  liftIO $ putStrLn $ "FAILED: result does't satisfy the condition: " ++ show a

shouldThrow :: Exception e => IO a -> (e -> Bool) -> IO ()
shouldThrow m f = do
  me <- try m
  case me of
    Right _ -> putStrLn "FAILED: expected to throw"
    Left e -> do
      unless (f e) $ do
        putStrLn $ "FAILED: exception doesn't satisfy the condition " ++ show e
