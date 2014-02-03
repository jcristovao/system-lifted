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

-- | This module is incomplete, it does not map all System.Posix.User.
-- Feel free to complete it.
module System.Posix.User.Lifted
  ( SystemPosixUser (..)
  , deriveSystemPosixUser
  , U.UserEntry(..)
  ) where

import System.Lifted
import qualified System.Posix.User as U
import System.Posix.Types

import Language.Haskell.TH

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

-- | System.Posix.User as a Class.
-- Instances for MaybeT and EitherT (String|Text|IOException|())
class SystemPosixUser e where
  getRealUserID               :: e IO UserID
  getRealGroupID              :: e IO GroupID
  getEffectiveUserID          :: e IO UserID
  getEffectiveGroupID         :: e IO GroupID
  getUserEntryForID           :: UserID -> e IO U.UserEntry

deriveSystemPosixUser :: Name -> DecsQ
deriveSystemPosixUser tp = let
    nm = conT tp
  in [d|

    instance SystemPosixUser $nm where
      getRealUserID          = ioT    U.getRealUserID
      getRealGroupID         = ioT    U.getRealGroupID
      getEffectiveUserID     = ioT    U.getEffectiveUserID
      getEffectiveGroupID    = ioT    U.getEffectiveGroupID
      getUserEntryForID      = ioT .  U.getUserEntryForID
  |]


