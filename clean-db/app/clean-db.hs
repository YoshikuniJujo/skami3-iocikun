module Main where

import Control.Monad.IO.Class
import Data.Time

import Database.Persist.TH
import Database.Esqueleto

import qualified Data.Text as Txt

import Template

tables "migrateAll" [persistLowerCase|
OpenIdStateNonce
	state	Txt.Text
	nonce	Txt.Text
	date	UTCTime
	deriving Show

Session
	session	Txt.Text
	userId	Txt.Text
	date	UTCTime
	deriving Show
|]

main :: IO ()
main = do
	t <- getCurrentTime
	runDB migrateAll $ do
		delete . from $ \sn -> where_ $
			sn ^. OpenIdStateNonceDate <. val (addUTCTime (- 300) t)
		selectAll @OpenIdStateNonce >>= liftIO . print
		delete . from $ \ssn -> where_ $
			ssn ^. SessionDate <. val (addUTCTime (- 1800) t)
		selectAll @Session >>= liftIO . print
