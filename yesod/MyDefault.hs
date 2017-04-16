module MyDefault (
	App(..), Handler, Route(..), Widget, myDefaultLayout, resourcesApp
	) where

--

import Import.NoFoundation hiding ((.), print, (==.), (=.), update, delete)

import Prelude
import Yesod.Core.Types (Logger)
import Data.Text (Text)
import Database.Esqueleto
import Web.Cookie

import Crypto.Random

import ClassyPrelude.Yesod (Static, Manager)

import Text.Hamlet (hamletFile)

import qualified Data.Text as Txt
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64.URL as B64

import Settings.StaticFiles (css_bootstrap_css)
import Yesod.Auth (Auth, getAuth)

mkYesodData "App" $(parseRoutesFile "config/routes")

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

myDefaultLayout :: (Yesod App, ToWidget App a) => a -> Handler Html
myDefaultLayout widget = do
	master <- getYesod
	session <- lookupCookie "session"
	autoLogin <- lookupCookie "auto-login"

	uid1 <- flip (maybe $ return []) session $ \ssn ->
		runDB $ select . from $ \s -> do
			where_ $ s ^. SessionSession ==. (val ssn)
			return $ s ^. SessionUserId

	mUserId <- case uid1 of
		[Value u] -> return $ Just u
		_ -> do	uid2 <- flip (maybe $ return []) autoLogin $ \al ->
				runDB $ select . from $ \a -> do
					where_ $ a ^. AutoLoginAutoLogin ==.
						(val al)
					return $ a ^. AutoLoginUserId
			case uid2 of
				[Value u'] -> do
					makeSession u'
					updateAutoLogin u'
					return $ Just u'
				_ -> return Nothing
	names <- flip (maybe $ return []) mUserId $ \u ->
		runDB . select . from $ \p -> do
			where_ $ p ^. ProfileUserId ==. val u
			return $ p ^. ProfileName
	lift $ print names
	let	(loginOut, loginOutLn) = case mUserId of
			Just _ -> ("logout" :: Text, "/ylogout" :: Text)
			Nothing -> ("login", "/ylogin")
		userName = case names of
			[Value n] -> n
			_ -> "ゲスト"

	pc <- widgetToPageContent $ do
		addStylesheet $ StaticR css_bootstrap_css
		$(widgetFile "default-layout")
	withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

makeSession :: Text -> Handler ()
makeSession uid = do
	ssn <- lift (getNonce 256)
	now <- liftIO getCurrentTime
	_ <- runDB $ do
		delete . from $ \s ->
			where_ $ s ^. SessionUserId ==. val uid
		insert $ Session ssn uid now
	setCookie def {
		setCookieName = "session",
		setCookieValue = TE.encodeUtf8 ssn,
		setCookiePath = Just "/",
		setCookieExpires = Nothing,
		setCookieMaxAge = Just 1800,
		setCookieDomain = Nothing,
		setCookieHttpOnly = True,
#ifdef DEVELOPMENT
		setCookieSecure = False,
#else
		setCookieSecure = True,
#endif
		setCookieSameSite = Just sameSiteStrict
		}

updateAutoLogin :: Text -> Handler ()
updateAutoLogin uid = do
	al <- lift (getNonce 512)
	_ <- runDB $ do
		update $ \a -> do
			set a [ AutoLoginAutoLogin =. val al ]
			where_ (a ^. AutoLoginUserId ==. val uid)
	setCookie def {
		setCookieName = "auto-login",
		setCookieValue = TE.encodeUtf8 al,
		setCookiePath = Just "/",
		setCookieExpires = Nothing,
		setCookieMaxAge = Just 2592000,
		setCookieDomain = Nothing,
		setCookieHttpOnly = True,
#ifdef DEVELOPMENT
		setCookieSecure = False,
#else
		setCookieSecure = True,
#endif
		setCookieSameSite = Just sameSiteStrict
		}

getNonce :: MonadRandom m => Int -> m Text
getNonce = (Txt.dropWhileEnd (== '=') . TE.decodeUtf8 . B64.encode <$>)
	. getRandomBytes

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
