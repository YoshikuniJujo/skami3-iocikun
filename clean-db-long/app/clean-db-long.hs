module Main where

import Control.Monad.IO.Class
import Data.Time
import Database.Persist.TH
import Database.Esqueleto

import qualified Data.Text as Txt

import Template

tables "migrateAll" [persistLowerCase|
AutoLogin
	autoLogin	Txt.Text
	userId		Txt.Text
	date		UTCTime
	deriving Show
|]

main :: IO ()
main = do
	t <- getCurrentTime
	runDB migrateAll $ do
		delete . from $ \al -> where_ $
			al ^. AutoLoginDate <. val (addUTCTime (- 2592000) t)
		selectAll @AutoLogin >>= liftIO . print
