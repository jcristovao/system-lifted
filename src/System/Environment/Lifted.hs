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

module System.Environment.Lifted (
    SystemEnvironment (..)
  , deriveSystemEnvironment
) where

import System.Lifted
import qualified System.Environment as E

import Language.Haskell.TH

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

-- | System.Environment as a Class.
-- Instances for MaybeT and EitherT (String|Text|IOException|())
class SystemEnvironment e where
  getArgs            :: e IO [String]
  getProgName        :: e IO String
  getExecutablePath  :: e IO FilePath
  getEnv             :: String -> e IO String
  -- special case?
  lookupEnv          :: String -> e IO (Maybe String)
  withArgs           :: [String] -> IO a -> e IO a
  withProgName       :: String -> IO a -> e IO a
  getEnvironment     :: e IO [(String, String)]


deriveSystemEnvironment :: Name -> DecsQ
deriveSystemEnvironment tp = let
    nm = conT tp
  in [d|

    instance SystemEnvironment $nm where
      getArgs           = ioT    E.getArgs
      getProgName       = ioT    E.getProgName
      getExecutablePath = ioT    E.getExecutablePath
      getEnv            = ioT .  E.getEnv
      lookupEnv         = ioT .  E.lookupEnv
      withArgs          = ioT .: E.withArgs
      withProgName      = ioT .: E.withProgName
      getEnvironment    = ioT    E.getEnvironment
  |]


