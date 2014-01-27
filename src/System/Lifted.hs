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
) where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity

import Data.Functor.Identity

import Data.Typeable
import GHC.IO.Exception
import System.IO.Error
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

class ToT n t m a | n -> t, t -> n where
  toT :: m (n a) -> t m a

instance ToT Maybe MaybeT IO a where
  toT = MaybeT

instance ToT (Either b) (EitherT b) IO a where
  toT = EitherT

instance ToT Identity IdentityT IO a where
  toT = IdentityT . fmap runIdentity

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

-- | TODO: instead of deriving, we should have 'run width'.



