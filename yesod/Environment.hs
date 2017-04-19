module Environment (
	ClientId, getClientId, cidToBs, cidToTxt,
	ClientSecret, getClientSecret, csToBs,
	RedirectUri, getRedirectUri, ruToBs, ruToTxt
	) where

import Prelude (Show, Eq, (.), (<$>), (++))

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.IO (IO, FilePath)
import System.FilePath ((</>))

import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt

directory :: FilePath
directory = "/home/tatsuya/keter/skami3/"

getRaw :: FilePath -> IO Text
getRaw = (Txt.concat . Txt.lines <$>)
	. Txt.readFile . (directory </>) . (++ ".txt")

newtype ClientId = ClientId { cidToTxt :: Text } deriving (Show, Eq)

getClientId :: IO ClientId
getClientId = ClientId <$> getRaw "clientId"

cidToBs :: ClientId -> ByteString
cidToBs = encodeUtf8 . cidToTxt

newtype ClientSecret = ClientSecret { csToTxt :: Text }

getClientSecret :: IO ClientSecret
getClientSecret = ClientSecret <$> getRaw "clientSecret"

csToBs :: ClientSecret -> ByteString
csToBs = encodeUtf8 . csToTxt

newtype RedirectUri = RedirectUri { ruToTxt :: Text }

getRedirectUri :: IO RedirectUri
getRedirectUri = RedirectUri <$> getRaw "redirectUri"

ruToBs :: RedirectUri -> ByteString
ruToBs = encodeUtf8 . ruToTxt
