{-# LANGUAGE OverloadedStrings #-}
import qualified Network.HTTP.Conduit as Http
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.UTF8 as UTF
import qualified Data.Maybe as Mb
import qualified Control.Monad as M
import qualified Network
import qualified Network.HTTP.Base as HttpBase
import qualified System.Environment as Env
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TEnc
import Data.Text.Lazy (Text(..))
import qualified Data.Aeson as JS
import Data.Aeson ((.:), (.:?), (.=))
import Data.Foldable (forM_)
import Control.Applicative ((<$>), (<*>))

data Result = Result {
    results :: [Entry],
    version :: Text
} deriving Show

data Entry = Entry {
    location :: Text,
    self :: Text,
    docs  :: Text
} deriving Show

instance JS.FromJSON Result where
    parseJSON (JS.Object v) = Result <$>
        v .: "results" <*>
        v .: "version"
    parseJSON _ = M.mzero

instance JS.FromJSON Entry where
    parseJSON (JS.Object v) = Entry <$>
        v .: "location" <*>
        v .: "self" <*>
        v .: "docs"
    parseJSON _ = M.mzero

main :: IO ()
main = Network.withSocketsDo $ do
    request <- Http.parseUrl "https://www.haskell.org/hoogle/"
    argv <- Env.getArgs
    let query = Just $ head $ UTF.fromString <$> argv
    let count = Just $ UTF.fromString $ show 3
    let start = Just $ UTF.fromString $ show 1
    let req = Http.setQueryString [("mode",Just $ UTF.fromString "json"),("hoogle",query),("start",start),("count",count)] request {
        Http.method = "GET",
        Http.secure = True
    }
    manager <- Http.newManager Http.tlsManagerSettings
    raw_json <- Http.httpLbs req manager
    let entry_list = results <$> ((JS.decode $ Http.responseBody raw_json) :: Maybe Result)
    forM_ (Mb.fromMaybe [] entry_list) $ \x -> do
        BS.putStr $ TEnc.encodeUtf8 $ self x
        BS8.putStrLn ""
        BS.putStr $ TEnc.encodeUtf8 $ docs x
        BS8.putStrLn ""
        BS8.putStrLn ""
