{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module System.Directory.Either where

import qualified System.Directory as D
import Network.HTTP.Conduit
{-import Control.Monad-}
import Control.Exception
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
{-import Control.Applicative-}
import Language.Haskell.TH
import Data.Text (Text)
import qualified Data.Text as T

-- | Just like (@'$'@), but with higher precedence than (@'<>'@), but still lower
-- than (@'.'@). Similar to "Diagrams.Util" @'#'@, but without flipped arguments.
{-# INLINE (§) #-}
(§) :: (a -> b) -> a -> b
f § x =  f x
infixr 8 §

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

class HandlerList errs a where
  handlerList :: errs -> [Handler a]

instance HandlerList IOException String where
  handlerList _ = [Handler (\(e::IOException) -> return . show $ e)]

instance HandlerList IOException Text where
  handlerList _ = [Handler (\(e::IOException) -> return . T.pack . show $ e)]

instance HandlerList IOException IOException where
  handlerList _ = [Handler (\(e::IOException) -> return e)]

instance HandlerList IOException () where
  handlerList _ = [Handler (\(_::IOException) -> return ())]

instance HandlerList (IOException, HttpException) Text where
  handlerList _ = [ Handler (\(e::IOException) -> return . T.pack . show $ e)
                , Handler (\(e::HttpException)->return . T.pack . show $ e)
                ]

-- One possibility is to define it for MaybeT, and leave it open
-- to EitherT. Does it make sense to define it also for ListT?
-- But then [] would mean failure, wouldn't it?
-- [()] success?
handlerListIoUnit :: [Handler ()]
handlerListIoUnit = handlerList ioExcept

class Tries a b c | c -> a b where
  tries :: [Handler a] -> IO b -> IO c

instance Tries a b (Either a b) where
  tries handlers io = fmap Right io
              `catch` catchesHandler handlers

instance Tries a b (Maybe b) where
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

class IOT t m a where
  ioT :: m a -> t m a

instance IOT MaybeT IO a where
  ioT iof = toT $ tries handlerListIoUnit iof

instance IOT (EitherT String) IO a where
  ioT iof = toT $ tries (handlerList ioExcept) iof

instance IOT (EitherT Text) IO a where
  ioT iof = toT $ tries (handlerList ioExcept) iof

instance IOT (EitherT ()) IO a where
  ioT iof = toT $ tries (handlerList ioExcept) iof

instance IOT (EitherT IOException) IO a where
  ioT iof = toT $ tries (handlerList ioExcept) iof

-- class SystemDirectory e where e IO () ?
class SystemDirectory e where
  createDirectory :: FilePath -> e IO ()

deriveSystemDirectory :: Name -> DecsQ
deriveSystemDirectory tp = let
    nm = conT tp
  in [d|
    instance SystemDirectory $nm where
      createDirectory fp = ioT $ D.createDirectory fp
  |]


