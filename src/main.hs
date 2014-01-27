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

import System.Directory.Either

import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Exception

type EitherString = EitherT String
type EitherText   = EitherT Text

deriveSystemDirectoryErrors "DisallowIOE [HardwareFault]" ''MaybeT
deriveSystemDirectoryErrors "AllIOE" ''EitherString
deriveSystemDirectory ''MaybeT
deriveSystemDirectory ''EitherString

main :: IO ()
main = do
  m <- runMaybeT $ createDirectory "abc"
  r <- runEitherT $ bimapEitherT T.pack id $ createDirectory "cde"
  print m
  case r of
    Left s -> T.putStrLn s
    Right _ -> putStrLn "OK"
  putStrLn "end"

{-tries :: [Handler a] -> IO b -> IO (Either a b)-}
{-tries handlers io = fmap Right io `catch` catchesHandler handlers-}

