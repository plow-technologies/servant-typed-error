{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Typed.Error
  ( GetTypedError,
    PostTypedError,
    DeleteTypedError,
    PutTypedError,
    WithError,
    TypedHandler (..),
    throwTypedError,
    throwServantError,
    runTypedHandler,
    liftTypedError,
    TypedClientM,
    typedClient,
    runTypedClientM,
  )
where

import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor (Bifunctor (..))
import Data.SOP (I (I))
import Data.SOP.BasicFunctors (unI)
import Servant (Union, WithStatus (..), (:<|>) (..))
import Servant.API (Capture, HasStatus (..), JSON, ReqBody, StdMethod (DELETE, GET, POST, PUT), UVerb, Union, WithStatus (..), (:<|>) (..), (:>))
import Servant.API.UVerb (eject, inject)
import Servant.Client (ClientEnv, ClientError, runClientM)
import Servant.Client.Internal.HttpClient (ClientM (..))
import Servant.Server (Handler (..), ServerError, respond)

-- These are needed due to overlapping instances. See: https://github.com/haskell-servant/servant/issues/1431
newtype WithStatus200 a = WithStatus200 (WithStatus 200 a)

newtype WithStatus500 a = WithStatus500 (WithStatus 500 a)

instance ToJSON a => ToJSON (WithStatus200 a) where
  toJSON (WithStatus200 (WithStatus a)) = toJSON a

instance FromJSON a => FromJSON (WithStatus200 a) where
  parseJSON o = (WithStatus200 . WithStatus) <$> parseJSON o

instance ToJSON a => ToJSON (WithStatus500 a) where
  toJSON (WithStatus500 (WithStatus a)) = toJSON a

instance FromJSON a => FromJSON (WithStatus500 a) where
  parseJSON o = (WithStatus500 . WithStatus) <$> parseJSON o

instance HasStatus (WithStatus200 a) where
  type StatusOf (WithStatus200 a) = 200

instance HasStatus (WithStatus500 a) where
  type StatusOf (WithStatus500 a) = 500

type WithError err ty = '[WithStatus200 ty, WithStatus500 err]

type GetTypedError resp ty err = UVerb 'GET resp '[WithStatus200 ty, WithStatus500 err]

type PostTypedError resp ty err = UVerb 'POST resp '[WithStatus200 ty, WithStatus500 err]

type DeleteTypedError resp ty err = UVerb 'DELETE resp '[WithStatus200 ty, WithStatus500 err]

type PutTypedError resp ty err = UVerb 'PUT resp '[WithStatus200 ty, WithStatus500 err]

newtype TypedHandler e a = TypedHandler {unTypedHandler :: ExceptT (Either ServerError e) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadError (Either ServerError e))

throwTypedError :: e -> TypedHandler e a
throwTypedError = throwError . Right

throwServantError :: ServerError -> TypedHandler e a
throwServantError = throwError . Left

-- | Inside `TypedHandler` we can throw two different kinds of errors:
-- Either a ServerError, via `throwServantError` or a custom error via `throwTyped`
runTypedHandler :: TypedHandler e a -> Handler (Union '[WithStatus200 a, WithStatus500 e])
runTypedHandler (TypedHandler m) =
  Handler $
    either
      (either throwError (pure . inject . I . WithStatus500 . WithStatus))
      (pure . inject . I . WithStatus200 . WithStatus)
      =<< liftIO (runExceptT m)

-- | This function is subtly different to `runTypedHandler` in that it can be used to
-- instantiate a function `f :: MonadError e m => m a` to `Handler (Union '[WithStatus200 a, WithStatus500 e])`.
-- Any calls to `throwError` in `f` will get turned to `throwTyped` in `liftTypedError f`.
-- In case you also want to throw a `ServantError`, use `runTypedHandler` instead.
liftTypedError :: Functor m => ExceptT e m a -> m (Union '[WithStatus200 a, WithStatus500 e])
liftTypedError m =
  either (inject . I . WithStatus500 . WithStatus) (inject . I . WithStatus200 . WithStatus)
    <$> runExceptT m

newtype TypedClientM e a = TypedClientM {unTypedClientM :: ReaderT ClientEnv (ExceptT (Either ClientError e) IO) a}

class TypedClient a b where
  typedClient :: a -> b

instance (TypedClient a b, TypedClient a' b') => TypedClient (a :<|> a') (b :<|> b') where
  typedClient = bimap typedClient typedClient

instance (TypedClient a' b', TypedClient b a) => TypedClient (a -> a') (b -> b') where
  typedClient f = typedClient . f . typedClient

instance TypedClient a a where
  typedClient = id

instance TypedClient (ClientM (Union '[WithStatus200 a, WithStatus500 e])) (TypedClientM e a) where
  typedClient (ClientM (ReaderT m)) = TypedClientM $
    ReaderT $ \env ->
      ExceptT $
        runExceptT (m env)
          >>= pure . \case
            Left servantErr -> Left $ Left servantErr
            Right u ->
              case unI <$> eject u of
                Just (WithStatus500 (WithStatus err)) -> Left $ Right err
                _ -> case unI <$> eject u of
                  Just (WithStatus200 (WithStatus a)) -> Right a
                  _ -> error "Didn't match on either response"

runTypedClientM :: TypedClientM e a -> ClientEnv -> IO (Either (Either ClientError e) a)
runTypedClientM cm env = runExceptT $ flip runReaderT env $ unTypedClientM cm
