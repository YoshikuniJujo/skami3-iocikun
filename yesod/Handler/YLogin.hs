module Handler.YLogin where

import Import (
	Handler, Html,
	uncurry, lift, getClientId, getRedirectUri,
	(=<<), (<$>), (<*>) )
import OpenIdCon (yconnect)

getYLoginR :: Handler Html
getYLoginR = uncurry yconnect =<< lift ((,) <$> getClientId <*> getRedirectUri)
