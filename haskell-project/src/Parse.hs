{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Parse
Description : This module includes all the functions necessary to parse data from the web into structures used by this app
-}
module Parse (
    -- * Functions
    parseRecords,
) where


import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

-- | The 'parseRecords' function turns the received raw json data into either a String or a Records structure
parseRecords :: L8.ByteString -> Either String Records 
parseRecords jsonData = eitherDecode jsonData :: Either String Records