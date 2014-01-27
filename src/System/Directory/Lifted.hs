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

module System.Directory.Lifted
  ( SystemDirectory (..)
  , Permissions (..)
  , deriveSystemDirectory
  ) where

import System.Lifted
import qualified System.Directory as D
import System.Directory (Permissions(..))
import Data.Time.Clock

import Language.Haskell.TH

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

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


