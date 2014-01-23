{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import System.Directory.Either

import qualified System.Directory as D
import Network.HTTP.Conduit
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Error.Util
import Control.Applicative
import Language.Haskell.TH
import Data.Text (Text)
import qualified Data.Text as T

import Language.Haskell.TH.Syntax

type EitherString = EitherT String

deriveSystemDirectory ''MaybeT
deriveSystemDirectory ''EitherString
{-deriveSystemDirectory [''EitherTStr ]-}
{-deriveSystemDirectory (EitherT String)-}

main :: IO ()
main = do
  m <- runMaybeT $ createDirectory "abc"
  r <- runEitherT $ createDirectory "cde"
  print m
  case r of
    Left s -> putStrLn s
    Right _ -> print "OK"
  print "end"

{-tries :: [Handler a] -> IO b -> IO (Either a b)-}
{-tries handlers io = fmap Right io `catch` catchesHandler handlers-}


