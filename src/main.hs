{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

import System.Lifted
import System.Directory.Lifted
import qualified System.Directory as D

import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Exception

type EitherString = EitherT String
type EitherText   = EitherT Text

deriveSystemLiftedErrors "DisallowIOE [HardwareFault]" ''MaybeT
deriveSystemLiftedErrors "AllIOE" ''EitherString
deriveSystemDirectory ''MaybeT
deriveSystemDirectory ''EitherString

main :: IO ()
main = do
  m <- runMaybeT $ createDirectory "abc"
  r <- runEitherT $ bimapEitherT T.pack id $ do
    createDirectory "cde"
    createDirectory "cdd"
  print m
  case r of
    Left s -> T.putStrLn s
    Right _ -> putStrLn "OK"

  j <- runEitherT $ ioFilterT (DisallowIOE [HardwareFault]) $ do
    D.createDirectory "ajj"
    D.createDirectory "aji"
  case j of
    Left s -> Prelude.putStrLn s
    Right _ -> putStrLn "OK!"
  print j
  putStrLn "end"

{-tries :: [Handler a] -> IO b -> IO (Either a b)-}
{-tries handlers io = fmap Right io `catch` catchesHandler handlers-}

