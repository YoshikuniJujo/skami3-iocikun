module Environment (
	directory,
	ClientId(..), getClientId,
	RedirectUri(..), getRedirectUri
	) where

import Prelude ((.), (<$>))

import System.IO
import System.FilePath ((</>))

import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt

directory :: FilePath
directory = "/home/tatsuya/keter/skami3/"

newtype ClientId = ClientId Text
newtype RedirectUri = RedirectUri Text

getClientId :: IO ClientId
getClientId = ClientId . Txt.concat . Txt.lines
	<$> Txt.readFile (directory </> "clientId.txt")

getRedirectUri :: IO RedirectUri
getRedirectUri = RedirectUri . Txt.concat . Txt.lines
		<$> Txt.readFile (directory </> "redirectUri.txt")
