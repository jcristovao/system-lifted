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
module System.Directory.Either (
    HandlerList (..)
  , SystemDirectory (..)
  , Permissions (..)
  , deriveSystemDirectory
  , deriveSystemDirectoryUnit
  , deriveSystemDirectoryErrors
  , deriveSystemDirectoryErrorsUnit
  , IOExceptionHandling (..)
) where

import Data.Text (Text)
import qualified Data.Text as T

import qualified System.Directory as D
import System.Directory (Permissions(..))
import Data.Time.Clock

import Network.HTTP.Conduit

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

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

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

ioExcept :: IOException
ioExcept = undefined

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

instance HandlerList (IOException, HttpException) Text where
  handlerList _ = [ Handler (\(e::IOException) -> return . T.pack . show $ e)
                , Handler (\(e::HttpException)->return . T.pack . show $ e)
                ]

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

evalTHStr :: String -> Q Exp
evalTHStr = return . either (\_ -> error "Error in template haskell") id
                   . parseExp

deriveSystemDirectoryErrors :: String -> Name -> DecsQ
deriveSystemDirectoryErrors ioh tp = let
  nm = conT tp
  iohv = evalTHStr ioh
  in [d|
        instance IOT $nm IO a where
          ioT iof = toT $ tries (handlerList (processIOExcepts $iohv)) iof
        |]

-- | TODO: instead of deriving, we should have 'run width'.

-- | System.Directory as a Class.
-- Instances for MaybeT and EitherT (String|Text|IOException|())
class SystemDirectory e where
  createDirectory             :: FilePath -> e IO ()
  createDirectoryIfMissing    :: Bool -> FilePath -> e IO ()
  removeDirectory             :: FilePath -> e IO ()
  removeDirectoryRecursive    :: FilePath -> e IO ()
  renameDirectory             :: FilePath -> FilePath -> e IO ()
  getDirectoryContents        :: FilePath -> e IO [FilePath]
  getCurrentDirectory         :: e IO FilePath
  setCurrentDirectory         :: FilePath -> e IO ()
  getHomeDirectory            :: e IO FilePath
  getAppUserDataDirectory     :: String -> e IO FilePath
  getUserDocumentsDirectory   :: e IO FilePath
  getTemporaryDirectory       :: e IO FilePath
  removeFile                  :: FilePath -> e IO ()
  renameFile                  :: FilePath -> FilePath -> e IO ()
  copyFile                    :: FilePath -> FilePath -> e IO ()
  canonicalizePath            :: FilePath -> e IO FilePath
  makeRelativeToCurrentDirectory :: FilePath -> e IO FilePath
  -- this may deserve a special case...
  findExecutable              :: String -> e IO (Maybe FilePath)
  findFile                    :: [FilePath] -> String -> e IO (Maybe FilePath)
  doesFileExist               :: FilePath -> e IO Bool
  doesDirectoryExist          :: FilePath -> e IO Bool
  getPermissions              :: FilePath -> e IO Permissions
  setPermissions              :: FilePath -> Permissions -> e IO ()
  copyPermissions             :: FilePath -> FilePath -> e IO ()
  getModificationTime         :: FilePath -> e IO UTCTime



deriveSystemDirectory :: Name -> DecsQ
deriveSystemDirectory tp = let
    nm = conT tp
  in [d|

    instance SystemDirectory $nm where
      createDirectory            = ioT .  D.createDirectory
      createDirectoryIfMissing   = ioT .: D.createDirectoryIfMissing
      removeDirectory            = ioT .  D.removeDirectory
      removeDirectoryRecursive   = ioT .  D.removeDirectoryRecursive
      renameDirectory orig       = ioT .  D.renameDirectory orig
      getDirectoryContents       = ioT .  D.getDirectoryContents
      getCurrentDirectory        = ioT    D.getCurrentDirectory
      setCurrentDirectory        = ioT .  D.setCurrentDirectory
      getHomeDirectory           = ioT    D.getHomeDirectory
      getAppUserDataDirectory    = ioT .  D.getAppUserDataDirectory
      getUserDocumentsDirectory  = ioT    D.getUserDocumentsDirectory
      getTemporaryDirectory      = ioT    D.getTemporaryDirectory
      removeFile                 = ioT .  D.removeFile
      renameFile                 = ioT .: D.renameFile
      copyFile                   = ioT .: D.copyFile
      canonicalizePath           = ioT .  D.canonicalizePath
      makeRelativeToCurrentDirectory = ioT . D.makeRelativeToCurrentDirectory
      findExecutable             = ioT .  D.findExecutable
      findFile                   = ioT .: D.findFile
      doesFileExist              = ioT .  D.doesFileExist
      doesDirectoryExist         = ioT .  D.doesDirectoryExist
      getPermissions             = ioT .  D.getPermissions
      setPermissions             = ioT .: D.setPermissions
      copyPermissions            = ioT .: D.copyPermissions
      getModificationTime        = ioT .  D.getModificationTime
  |]


