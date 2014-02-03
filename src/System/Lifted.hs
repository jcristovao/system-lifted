{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module System.Lifted (
    HandlerList (..)
  , IOT (..)
  , deriveSystemLiftedErrors
  , IOExceptionHandling (..)
  , joinMaybeMT
  , joinMMT
  , joinMaybeET
  , joinMET
  , joinMaybeErrT
  , joinMErrT
  , joinEitherET
  , joinEET
  , errorTtoEitherT
  , evalEither
  , evalEitherT
  , reportEither
  , reportEitherT
  , showStr
) where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable

import Data.Functor.Identity
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Control.Monad.Trans.Error


import GHC.IO.Exception
import System.IO.Error
import System.Exit
import Control.Exception

import Language.Haskell.TH
import Language.Haskell.Meta.Parse

-- | Convert an Either value to a Maybe value
--
-- This function is provided with a different name convention on
-- @Data.Either.Combinators@:
--
-- @
-- 'eitherToMaybe' = 'rightToMaybe'
-- @
--
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

data IOExceptionHandling = AllowIOE    [IOErrorType]
                         | DisallowIOE [IOErrorType]
                         | AllIOE
                         deriving (Eq,Typeable)

instance Show IOExceptionHandling where
  show (AllowIOE  _)   = "AllIOE"
  show (DisallowIOE _) = "DisallowIOE"
  show AllIOE          = "AllIOE"


processIOExcepts :: IOExceptionHandling -> IOException -> IOException
processIOExcepts ioeh ioe =
  case ioeh of
    -- | Provided a list of IOExceptions to allow (reported as Left or Nothing),
    -- throw a IOException if not in this list (White Listing).
    AllowIOE lst -> if ioeGetErrorType ioe `elem` lst
                      then ioe
                      else throw ioe

    -- | Provided a list of IOExceptions to forbid, rethrow a IOException if
    -- mentioned in this list (Black Listing).
    DisallowIOE lst -> if ioeGetErrorType ioe `elem` lst
                      then throw ioe
                      else ioe

    -- | No exception filtering done, all reported as Left or Nothing
    AllIOE -> ioe

-- | Do not rethrow any IO Exception
{-allowIOExcepts :: [IOErrorType] -> IOException -> IOException-}
{-allowIOExcepts _ ioe = ioe-}

class HandlerList errs a where
  handlerList :: errs -> [Handler a]

instance HandlerList (IOException -> IOException) String where
  handlerList f = [Handler (\(e::IOException) -> return . show . f $ e)]

instance HandlerList (IOException -> IOException) () where
  handlerList f = [Handler (\(e::IOException) -> evaluate (f e) >> return ())]

instance HandlerList (IOException -> IOException) Text where
  handlerList f = [Handler (\(e::IOException) -> return . T.pack . show . f $ e)]

instance HandlerList (IOException -> IOException) IOException where
  handlerList f = [Handler (\(e::IOException) -> return . f $ e)]


-- One possibility is to define it for MaybeT, and leave it open
-- to EitherT. Does it make sense to define it also for ListT?
-- But then [] would mean failure, wouldn't it?
-- [()] success?
handlerListIoUnit :: IOExceptionHandling -> [Handler ()]
handlerListIoUnit f = handlerList (processIOExcepts f)

class Tries a b c | c -> a b where
  tries :: [Handler a] -> IO b -> IO c

instance Tries a b (Identity b) where
  tries _ io = fmap Identity io

instance Tries a b (Either a b) where
  tries handlers io = fmap Right io
              `catch` catchesHandler handlers

instance Tries () b (Maybe b) where
  tries handlers io = fmap Just io
             `catch` (fmap eitherToMaybe . catchesHandler handlers)

catchesHandler :: [Handler a] -> SomeException -> IO (Either a b)
catchesHandler handlers e = foldr tryHandler (throw e) handlers
    where tryHandler (Handler handler) res =
            case fromException e of
              Just e' -> fmap Left (handler e')
              Nothing -> res

class ToT n t m a | t -> n where
  toT :: m (n a) -> t m a

instance ToT Maybe MaybeT IO a where
  toT = MaybeT

instance ToT (Either b) (EitherT b) IO a where
  toT = EitherT

instance ToT Identity IdentityT IO a where
  toT = IdentityT . fmap runIdentity

instance ToT (Either b) (ErrorT b) IO a where
  toT = ErrorT

class IOT t m a where
  ioT :: m a -> t m a
  ioFilterT :: IOExceptionHandling -> m a -> t m a

evalTHStr :: String -> Q Exp
evalTHStr = return . either (\_ -> error "Error in template haskell") id
                   . parseExp

-- | The io handlers is passed as string due to TH peculiarities (and
-- my lack of TH knoledge, mainly).
deriveSystemLiftedErrors :: String -> Name -> DecsQ
deriveSystemLiftedErrors ioh tp = let
  nm = conT tp
  iohv = evalTHStr ioh
  in [d|
        instance IOT $nm IO a where
          ioT iof = toT $ tries (handlerList (processIOExcepts $iohv)) iof
          ioFilterT ioeh iof = toT $ tries (handlerList (processIOExcepts ioeh)) iof
        |]

joinMaybeMT, joinMMT :: MaybeT IO (Maybe a) -> MaybeT IO a
joinMMT = joinMaybeMT
joinMaybeMT mbt = do
  mb <- liftIO $ runMaybeT mbt
  MaybeT . return $ maybe Nothing (maybe Nothing Just) mb


joinMaybeET, joinMET :: b -> EitherT b IO (Maybe a) -> EitherT b IO a
joinMET = joinMaybeET
joinMaybeET e mbt = do
  mb <- liftIO $ runEitherT mbt
  hoistEither $ either Left (maybe (Left e) Right) mb

joinEitherET, joinEET :: EitherT b IO (Either b a) -> EitherT b IO a
joinEET = joinEitherET
joinEitherET eit = hoistEither . join =<< (liftIO . runEitherT $ eit)

joinMaybeErrT, joinMErrT :: (Error b)
  => b -> ErrorT b IO (Maybe a) -> ErrorT b IO a
joinMErrT = joinMaybeErrT
joinMaybeErrT e mbt = do
  mb <- liftIO $ runErrorT mbt
  ErrorT . return $ either Left (maybe (Left e) Right) mb

errorTtoEitherT :: ErrorT a IO b -> EitherT a IO b
errorTtoEitherT = EitherT . runErrorT

-- | Evaluate an @Either IOException a@ to either raise the exception
-- or return the right value
evalEither :: Either IOException a -> IO a
evalEither ei = case ei of
    Left e  -> throwIO e
    Right p -> return p

-- | Evaluate an @EitherT IOException IO a@ to either raise the exception
-- or return the right value
evalEitherT :: EitherT IOException IO a -> IO a
evalEitherT eit = runEitherT eit >>= evalEither

-- | Show a string like value (@String@ or @Text@) without the annoying quotes
showStr :: (IsString s, Show s) => s -> String
showStr = Prelude.init . Prelude.tail . show

-- | Either return the right value or print the error message in the @Left@ value
-- and exit
reportEither :: (IsString e, Show e) => Either e a -> IO a
reportEither ei = case ei of
  Left  e -> (putStrLn . showStr $ e) >> exitFailure
  Right p -> return p

-- | Either return the right value or print the error message in the @Left@ value
-- and exit
reportEitherT :: (IsString e, Show e) => EitherT e IO a -> IO a
reportEitherT eit = runEitherT eit >>= reportEither


