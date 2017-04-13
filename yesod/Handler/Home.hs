module Handler.Home where

import Import (
	Handler, Html,
	uncurry, lift, getClientId, getRedirectUri,
	(=<<), (<$>), (<*>) )
import OpenIdCon (yconnect)

getHomeR :: Handler Html
getHomeR = uncurry yconnect =<< lift ((,) <$> getClientId <*> getRedirectUri)
