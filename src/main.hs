{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import System.Directory.Either

import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe

type EitherString = EitherT String

deriveSystemDirectoryUnit ''MaybeT
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


