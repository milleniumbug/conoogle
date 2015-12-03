{-# LANGUAGE OverloadedStrings #-}
import qualified Network.HTTP.Conduit as Http
import qualified Data.ByteString as BS
import qualified Network
import qualified Text.HTML.TagSoup as TS

main :: IO ()
main = Network.withSocketsDo $ do
    request <- Http.parseUrl "https://www.haskell.org/hoogle/"
    let req = request {
        Http.method = "GET",
        Http.secure = True,
        Http.queryString = "hoogle?=Data.Map"
    }
    manager <- Http.newManager Http.tlsManagerSettings
    result <- Http.httpLbs request manager
    print $ TS.parseTags $ Http.responseBody result
