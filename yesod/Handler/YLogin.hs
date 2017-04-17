module Handler.YLogin where

import Import (Handler, Html, uncurry, lift, (=<<), (<$>), (<*>))
import OpenIdCon (yconnect)
import Environment (getClientId, getRedirectUri)

getYLoginR :: Handler Html
getYLoginR = uncurry yconnect =<< lift ((,) <$> getClientId <*> getRedirectUri)
