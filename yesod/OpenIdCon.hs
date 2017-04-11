module OpenIdCon (
	yconnect
	) where

import Import

import Crypto.Random

import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Text as Txt

getNonce :: MonadRandom m => Int -> m Text
getNonce = (Txt.dropWhileEnd (== '=') . decodeUtf8 . B64.encode <$>)
	. getRandomBytes

yconnect :: ClientId -> RedirectUri -> Handler Html
yconnect (ClientId cid) (RedirectUri ruri) = do
	(state, nonce, date) <- lift
		$ (,,) <$> getNonce 256 <*> getNonce 256 <*> getCurrentTime
	_ <- runDB $ insert $ OpenIdStateNonce state nonce date
	runDB (selectList ([] :: [Filter OpenIdStateNonce]) [])
		>>= mapM_ print
	yc state nonce
	where
	yc :: MonadHandler m => Text -> Text -> m a
	yc stt nnc = redirect $
		"https://auth.login.yahoo.co.jp/yconnect/v1/authorization?" <>
		"response_type=code+id_token&" <>
		"scope=openid+profile&" <>
		"client_id=" <> cid <> "&" <>
		"state=" <> stt <> "&" <>
		"nonce=" <> nnc <> "&" <>
		"redirect_uri=" <> ruri
