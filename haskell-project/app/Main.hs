{-|
Module      : Main
Description : This module includes all the main menu of this app
-}
module Main (
    -- * Function
    main
) where

import System.IO
import Types
import Fetch
import Parse
import Database
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as L8

-- |The 'main' function opens up the menu for the user to interact with the app
main :: IO ()
main = do
    putStrLn "============================="
    putStrLn "WELCOME TO THE MOVIE DATA APP"
    putStrLn "============================="
    putStrLn " 1. Download data"
    putStrLn " 2. Perform database queries"
    putStrLn " 3. Quit"
    putStrLn "============================="
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStrLn ""
    putStrLn "Please choose an option: "
    userChoice <- try (readLn) :: IO (Either SomeException Int)
    case userChoice of
        Left _ -> do
            print "Invalid option"
            main
        Right n -> do
            case n of
                1 -> do
                    let url = "https://champagnekap.github.io/data/movies.json"
                    -- let url = "https://raw.githubusercontent.com/pedroaccamara/pedroaccamara.github.io/main/movies.json" -- Quick tests
                    print "Downloading data..."
                    jsonData <- download url
                    if jsonData == L8.empty then do main
                    else do
                        print "Parsing data..."
                        case parseRecords jsonData of
                            Left _ -> do
                                print "Error: the data is not valid. Please check the link and try again."
                                main
                            Right recs -> do
                                print "Saving records to database..."
                                saveRecords conn (records recs)
                                print "Records saved successfully!"
                                main
                2 -> do
                    databaseQueries conn
                    main
                3 -> print "Thanks for using our app"
                _ -> do
                    print "Invalid option"
                    main

