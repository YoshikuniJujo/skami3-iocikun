module Handler.Home where

import Import
import OpenIdCon

getHomeR :: Handler Html
getHomeR = uncurry yconnect =<< lift ((,) <$> getClientId <*> getRedirectUri)
