module Main where

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
|]

main :: IO ()
main = do
	t <- getCurrentTime
	runDB migrateAll . delete . from $ \sn -> where_ $
		sn ^. OpenIdStateNonceDate <. val (addUTCTime (- 300) t)
