module Application (appMain) where

import           ClassyPrelude
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List            as C
import           Network.HTTP.Conduit
import           Prelude                      ()
import           System.Directory
import           Text.Read
import           Web.Authenticate.OAuth
import           Web.Twitter.Conduit
import           Web.Twitter.Types

appMain :: IO ()
appMain = do
    initDirectory
    manager <- newManager tlsManagerSettings
    twInfo <- getTWInfo manager
    putStrLn "# your home timeline (up to 800 tweets):"
    runResourceT $ sourceWithMaxId twInfo manager homeTimeline
        $= C.isolate 10
        $$ C.mapM_ $ \status -> putStrLn (statusText status)

initDirectory :: IO ()
initDirectory = createDirectoryIfMissing True =<< getQancttCacheDirectory

getQancttCacheDirectory :: IO FilePath
getQancttCacheDirectory = do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".cache" </> "qanctt"

getCredentialCachePath :: IO FilePath
getCredentialCachePath = do
    cacheDir <- getQancttCacheDirectory
    return $ cacheDir </> "credential"

getTWInfo :: Manager -> IO TWInfo
getTWInfo manager = do
    mCred <- getCredentialCache
    cred <- case mCred of
        Just cred -> return cred
        Nothing -> do
            tempCred <- getTemporaryCredential oauth manager
            oauthVerifier <- getPIN $ pack $ authorizeUrl oauth tempCred
            getAccessToken oauth (injectVerifier oauthVerifier tempCred) manager
    setCredentialCache cred
    return $ setCredential oauth cred def

getCredentialCache :: IO (Maybe Credential)
getCredentialCache = do
    oauthVerifierExist <- doesFileExist =<< getCredentialCachePath
    if oauthVerifierExist
        then readMaybe <$> (readFile =<< getCredentialCachePath)
        else return Nothing

setCredentialCache :: Credential -> IO ()
setCredentialCache cred = do
    credentialCachePath <- getCredentialCachePath
    writeFile credentialCachePath $ show cred

oauth :: OAuth
oauth = twitterOAuth
    { oauthConsumerKey    = "k6mFN00CgqUadJdNbdigsrTMh"
    , oauthConsumerSecret = "yD0rZIh5Cl6wsQpSZTHFpuO187xXzjUe0zOnzrqpOxJMxelgOg"
    }

getPIN :: Text -> IO ByteString
getPIN url = do
    putStrLn $ "browse URL: " <> url
    putStrLn "please input pin"
    getLine
