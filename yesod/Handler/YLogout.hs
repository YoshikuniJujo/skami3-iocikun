module Handler.YLogout (getYLogoutR) where

import Import (
	Handler, Html, Text,
	($), (.), (>>=), maybe, return, runDB, redirect, lookupCookie )
import Model (EntityField(..))
import Database.Esqueleto ((^.), (==.), delete, from, where_, val)

getYLogoutR :: Handler Html
getYLogoutR = do
	(lookupCookie "session" >>=) . maybe (return ()) $ \s ->
		runDB . delete . from
			$ where_ . (==. val s) . (^. SessionSession)
	(lookupCookie "auto-login" >>=) . maybe (return ()) $ \al ->
		runDB . delete . from
			$ where_ . (==. val al) . (^. AutoLoginAutoLogin)
	redirect ("/" :: Text)
