module Handler.Home where

import Import
import OpenIdCon (yconnect)

getHomeR :: Handler Html
getHomeR = uncurry yconnect =<< lift ((,) <$> getClientId <*> getRedirectUri)
