{-# LANGUAGE Rank2Types #-}

module Common (
	Random, rndToTxt, unstring, UserId(..), AccessToken(..), MyHandler,
	getRand, rndToBs, unnumber, EHandler
	) where

import Control.Monad.Trans.Except

import Import.NoFoundation hiding (UserId)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as B64

import Crypto.Random

import Data.Scientific

newtype Random = Random { rndToBs :: ByteString }

getRand :: MonadRandom m => Int -> m Random
getRand = (Random . fst . BSC.spanEnd (== '=') . B64.encode <$>)
	. getRandomBytes

rndToTxt :: Random -> Text
rndToTxt = decodeUtf8 . rndToBs

unstring :: Aeson.Value -> Maybe Text
unstring (String t) = Just t
unstring _ = Nothing

unnumber :: Aeson.Value -> Maybe Scientific
unnumber (Number n) = Just n
unnumber _ = Nothing

newtype UserId = UserId Text deriving Show
newtype AccessToken = AccessToken Text deriving Show

type MyHandler a = forall site . (
		BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryWrite (YesodPersistBackend site),
		PersistUniqueWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site ) => HandlerT site IO a

type EHandler a = forall site . (
		BaseBackend (YesodPersistBackend site) ~ SqlBackend,
		PersistQueryWrite (YesodPersistBackend site),
		PersistUniqueWrite (YesodPersistBackend site),
		IsPersistBackend (YesodPersistBackend site),
		YesodPersist site ) => ExceptT String (HandlerT site IO) a
