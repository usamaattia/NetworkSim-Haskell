{-|
Module      : Fetch
Description : This module includes all the functions necessary to fetch data from the web
-}
module Fetch (
    -- * Functions
    download
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Control.Exception

-- | The 'URL' type defines whats acceptable input for the 'download' function in a more readable way
type URL = String

-- | The 'download' function takes a URL of a page, and fetches and returns the json data present in that page. It checks whether the request gets a successful response.
download :: URL -> IO L8.ByteString
download url = do
    request <- parseRequest url
    res <- try (httpLBS request) :: IO (Either SomeException (Response L8.ByteString))
    case res of
        Left ex -> do
            print "Error: something went wrong. Please check your internet connection or the link, and try again."
            return L8.empty
        Right response -> do
            return $ getResponseBody response