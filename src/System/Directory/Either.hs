{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module System.Directory.Either where

import qualified System.Directory as D
import Network.HTTP.Conduit
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Error.Util
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

ioExcept :: IOException
ioExcept = undefined

class ErrorsToCatch errs a where
  handlerList :: errs -> [Handler a]

instance ErrorsToCatch IOException String where
  handlerList _ = [Handler (\(e::IOException) -> return . show $ e)]

instance ErrorsToCatch IOException Text where
  handlerList _ = [Handler (\(e::IOException) -> return . T.pack . show $ e)]

instance ErrorsToCatch IOException IOException where
  handlerList _ = [Handler (\(e::IOException) -> return e)]

instance ErrorsToCatch IOException () where
  handlerList _ = [Handler (\(e::IOException) -> return ())]

instance ErrorsToCatch (IOException, HttpException) Text where
  handlerList _ = [ Handler (\(e::IOException) -> return . T.pack . show $ e)
                , Handler (\(e::HttpException)->return . T.pack . show $ e)
                ]

class Tries a b c | c -> a b where
{-class Tries a b c | a b -> c where-}
{-class Tries a b c where-}
  tries :: [Handler a] -> IO b -> IO c

instance Tries a b (Either a b) where
  tries handlers io = fmap Right io
              `catch` catchesHandler handlers

instance Tries a b (Maybe b) where
  tries handlers io = fmap Just io
             `catch` (fmap eitherToMaybe . catchesHandler handlers)

{-instance Tries a () (Maybe ()) where-}
  {-tries handlers io = fmap Just io-}
             {-`catch` (fmap eitherToMaybe . catchesHandler handlers)-}


catchesHandler :: [Handler a] -> SomeException -> IO (Either a b)
catchesHandler handlers e = foldr tryHandler (throw e) handlers
    where tryHandler (Handler handler) res =
            case fromException e of
              Just e' -> fmap Left (handler e')
              Nothing -> res


{-io :: IO b -> EitherT String IO (Either String b)-}
{-io :: (MonadTrans t) => (Either e a -> b) -> IO a -> t IO b-}
{-io :: (Functor (t IO), MonadTrans t, ErrorsToCatch IOException a) => (Either a c -> b) -> IO c -> t IO b-}
{-io f = fmap f . lift . tries (handlerList ioExcept)-}

-- class SystemDirectory e where e IO () ?
class SystemDirectoryET e where
  createDirectory :: FilePath -> e IO ()

{-createDirectory' = io . D.createDirectory-}
handlerListIoUnit = handlerList ioExcept :: [Handler ()]

instance SystemDirectoryET (EitherT String) where
  createDirectory fp = (liftIO $ tries (handlerList ioExcept) (D.createDirectory fp)) >>= hoistEither

instance SystemDirectoryET MaybeT where
  createDirectory fp = (liftIO $ tries handlerListIoUnit (D.createDirectory fp)) >>= hoistMaybe

    {-hushT $ (createDirectory :: FilePath -> EitherT String IO ())  fp-}
    {-liftIO . tries (handlerList (undefined::IOException)) . D.createDirectory § fp >>= hoistMaybe-}


main = do
  _ <- runMaybeT $ createDirectory "abc"
  print "OK"

{-tries :: [Handler a] -> IO b -> IO (Either a b)-}
{-tries handlers io = fmap Right io `catch` catchesHandler handlers-}

-- | Just like (@'$'@), but with higher precedence than (@'<>'@), but still lower
-- than (@'.'@). Similar to "Diagrams.Util" @'#'@, but without flipped arguments.
{-# INLINE (§) #-}
(§) :: (a -> b) -> a -> b
f § x =  f x
infixr 8 §

-- | Transform a either value encapsulated in a @Monad m@ into the equivalent
-- MaybeT m monad transformer.
--
-- /Note/: The left value is silently discarded.
mEitherToMaybeT :: (Functor m) => m (Either a b) -> MaybeT m b
mEitherToMaybeT eF = MaybeT (eitherToMaybe <$> eF)

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

eitherToMaybe' :: Either a () -> Maybe ()
eitherToMaybe' = either (const Nothing) (const (Just ()))


