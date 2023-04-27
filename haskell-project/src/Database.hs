{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

{-|
Module      : Database
Description : This module includes all the main functions necessary to manipulate the database on a high-level
-}
module Database (
    -- * Functions
    initialiseDB,
    getOrCreateMovie,
    getLastElement,
    getOrCreateActor,
    iterateCast,
    getOrCreateGenre,
    iterateGenre,
    createRecord,
    saveRecords,
    databaseQueries,
    extractActorResults,
    extractGenreResults,
    queryMovie,
    queryActor,
    queryGenre,
    queryYear
) where

import Types
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Control.Exception

-- |The 'initialiseDB' function prepares a database in the `movies.sqlite` file to be used by the app
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "movies.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS actors (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \first_name VARCHAR(80), \
            \last_name VARCHAR(80) \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS movies (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \title VARCHAR(80) NOT NULL, \
            \year INT DEFAULT NULL \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS movie_genre (\
            \fk_movie_genre INTEGER, \
            \fk_genre_genre INTEGER \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS cast (\
            \fk_movie_cast INTEGER, \
            \fk_movie_actor INTEGER \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS genres (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \first_name VARCHAR(80), \
            \second_name VARCHAR(80) \
            \)"
        return conn

-- |The 'getOrCreateMovie' function ensures the unique movie title for that year exists in the database before returning it
getOrCreateMovie :: Connection -> String -> Int -> IO Movie
getOrCreateMovie conn tit year = do
    results <- queryNamed conn "SELECT * FROM movies WHERE title=:title AND year=:year" [":title" := tit, ":year" := year]
    if not (null results) then
        return . head $ results
    else do
        execute conn "INSERT INTO movies (title, year) VALUES (?, ?)" (tit, year)
        getOrCreateMovie conn tit year

getLastElement :: [String] -> String
-- ^The 'getLastElement' connects an array of strings by spaces
getLastElement [] = ""
getLastElement (x:xs) = x ++ " " ++ getLastElement xs

-- |The 'getOrCreateActor' function ensures the unique actor name exists in the database before returning it
getOrCreateActor :: Connection -> String -> IO Actor
getOrCreateActor conn name = do
    let actor_name = words name
    let first_name = head actor_name
    let last_name = getLastElement (tail actor_name)
    results <- queryNamed conn "SELECT * FROM actors WHERE first_name=:first_name AND last_name=:last_name" [":first_name" := first_name, ":last_name" := last_name]
    if not (null results) then
        return . head $ results
    else do
        execute conn "INSERT INTO actors (first_name, last_name) VALUES (?, ?)" (first_name, last_name)
        getOrCreateActor conn name

-- |The 'iterateCast' function ensures every actor in a given cast is saved into the database before associating it with a movie
iterateCast :: Connection -> Movie -> [String] -> IO ()
iterateCast conn m cast = do
    if null cast then return ()
    else do
        a <- getOrCreateActor conn (head cast)
        let mid = movie_id_ m
        let aid = actor_id_ a
        execute conn "INSERT INTO cast VALUES (?,?)" (mid, aid)
        iterateCast conn m (tail cast)

-- |The 'getOrCreateGenre' function ensures the unique genre exists in the database before returning it
getOrCreateGenre :: Connection -> String -> IO Genre
getOrCreateGenre conn genre = do
    let genre_name = words genre
    let first_name = head genre_name
    let second_name = getLastElement (tail genre_name)
    results <- queryNamed conn "SELECT * FROM genres WHERE first_name=:first_name AND second_name=:second_name" [":first_name" := first_name, ":second_name" := second_name]
    if not (null results) then
        return . head $ results
    else do
        execute conn "INSERT INTO genres (first_name, second_name) VALUES (?, ?)" (first_name, second_name)
        getOrCreateGenre conn genre

-- |The 'iterateGenre' function ensures every genres in a given list is saved into the database before associating it with a movie
iterateGenre :: Connection -> Movie -> [String] -> IO ()
iterateGenre conn m genres = do
    if null genres then return ()
    else do
        g <- getOrCreateGenre conn (head genres)
        let mid = movie_id_ m
        let gid = genre_id_ g
        execute conn "INSERT INTO movie_genre VALUES (?,?)" (mid, gid)
        iterateGenre conn m (tail genres)

-- |The 'createRecord' function saves the full record of a given movie into the database
createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    m <- getOrCreateMovie conn (title record) (year record)
    iterateCast conn m (cast record)
    iterateGenre conn m (genres record)
    
-- |The 'saveRecords' function saves the full record of all movies in a list of movies into the database
saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

-- |The 'databaseQueries' function opens the menu for the user to perform queries on the database
databaseQueries :: Connection -> IO ()
databaseQueries conn = do
    putStrLn "============================="
    putStrLn "    DATABASE QUERY OPTIONS"
    putStrLn "============================="
    putStrLn " 1. Get full details of any movie"
    putStrLn " 2. Get full details of any actor"
    putStrLn " 3. Get full details of any genre"
    putStrLn " 4. Get movies released in any year"
    putStrLn " 5. Back"
    putStrLn "============================="
    putStrLn ""
    putStrLn "Please choose an option: "
    queryChoice <- try readLn :: IO (Either SomeException Int)
    case queryChoice of
        Left ex -> do
            print "Invalid option"
            databaseQueries conn
        Right n -> do
            case n of
                1 -> do
                    movieDetails <- queryMovie conn
                    putStrLn "============================="
                    putStr movieDetails
                    putStrLn "============================="
                    databaseQueries conn
                2 -> do
                    actorDetails <- queryActor conn
                    putStrLn "============================="
                    putStrLn actorDetails
                    putStrLn "============================="
                    databaseQueries conn
                3 -> do
                    genreDetails <- queryGenre conn
                    putStrLn "============================="
                    putStrLn genreDetails
                    putStrLn "============================="
                    databaseQueries conn
                4 -> do
                    yearDetails <- queryYear conn
                    putStrLn "============================="
                    putStrLn yearDetails
                    putStrLn "============================="
                    databaseQueries conn
                5 -> do print "We hope you enjoyed querying!"
                _ -> do
                    print "Invalid option"
                    databaseQueries conn

-- |The 'extractActorResults' transforms an array of Actor's into a displayable string with the names of all the actors in the array
extractActorResults :: [Actor] -> String
extractActorResults = foldr (\ actorRec -> (++) (actor_first_name_ actorRec ++ " " ++ actor_last_name_ actorRec)) ""

-- |The 'extractGenreResults' transforms an array of Genre's into a displayable string with the names of all the genres in the array
extractGenreResults :: [Genre] -> String
extractGenreResults = foldr (\ genreRec -> (++) (genre_first_name_ genreRec ++ " " ++ genre_second_name_ genreRec)) ""

-- |The 'queryMovie' allows the user to see details of a movie with a specific title
queryMovie :: Connection -> IO String
queryMovie conn = do
    putStr "Enter movie title > "
    userInput <- try getLine :: IO (Either SomeException String)
    case userInput of
        Left ex -> do
            return "Invalid input"
        Right movieName -> do
            let sql1 = "SELECT DISTINCT * FROM movies m WHERE m.title = ?"
            movieResults <- query conn sql1 [movieName]
            if not (null movieResults) then do
                let sql2 = "SELECT DISTINCT a.* FROM actors a JOIN cast c ON a.id == c.fk_movie_actor JOIN movies m ON c.fk_movie_cast == m.id WHERE m.title = ?"
                actorResults <- query conn sql2 [movieName]
                let sql3 = "SELECT DISTINCT g.* FROM genres g JOIN movie_genre mg ON g.id == mg.fk_genre_genre JOIN movies m ON mg.fk_movie_genre == m.id WHERE m.title = ?"
                genreResults <- query conn sql3 [movieName]
                let movieTitle = title_ (head movieResults)
                let movieYear = year_ (head movieResults)
                let movieCast = extractActorResults actorResults
                let movieGenres = extractGenreResults genreResults
                return ("Title: " ++ movieTitle ++ "\nYear Released: " ++ show movieYear ++ "\nCast: " ++ movieCast ++ "\nGenres: " ++ movieGenres ++ "\n")
            else return "Error: this movie isn't in our database.\n"

-- |The 'queryActor' allows the user to see details of an actor with a specific name
queryActor :: Connection -> IO String
queryActor conn = do
    putStr "Enter actor name > "
    userInput <- try getLine :: IO (Either SomeException String)
    case userInput of
        Left ex -> do
            return "Invalid input"
        Right actorName -> do
            let actor_fn = head (words actorName)
            let actor_ln = getLastElement (tail (words actorName))
            let sql1 = "SELECT DISTINCT a.* FROM actors a WHERE a.first_name = ? AND a.last_name = ?"
            actorResults <- query conn sql1 (actor_fn, actor_ln) :: IO [Actor]
            if not (null actorResults) then do
                let sql2 = "SELECT DISTINCT m.* FROM movies m JOIN cast c ON m.id == c.fk_movie_cast JOIN actors a ON c.fk_movie_actor == a.id WHERE a.first_name = ? AND a.last_name = ? ORDER BY m.year DESC"
                movieResults <- query conn sql2 (actor_fn, actor_ln) :: IO [Movie]
                let sql3 = "SELECT DISTINCT g.* FROM genres g JOIN movie_genre mg ON g.id == mg.fk_genre_genre JOIN movies m ON mg.fk_movie_genre == m.id JOIN cast c ON m.id == c.fk_movie_cast JOIN actors a ON c.fk_movie_actor == a.id WHERE a.first_name = ? AND a.last_name = ?"
                genreResults <- query conn sql3 (actor_fn, actor_ln) :: IO [Genre]
                let a_fn = actor_first_name_ (head actorResults)
                let a_ln = actor_last_name_ (head actorResults)
                let num_genres = length genreResults
                let num_movies = length movieResults
                let output = "Name: " ++ a_fn ++ " " ++ a_ln ++ "\nNo. of movies: " ++ show num_movies ++ "\nNo. of genres: " ++ show num_genres ++ "\nLatest movie: "
                if num_movies /= 0 then do
                    let latest_movie = title_ (head movieResults)
                    return (output ++ latest_movie)
                else 
                    return (output ++ "None")            
            else return "Error: this actor isn't in our database."

-- |The 'queryGenre' allows the user to see details of an genre with a specific name
queryGenre :: Connection -> IO String
queryGenre conn = do
    putStr "Enter genre name > "
    userInput <- try getLine :: IO (Either SomeException String)
    case userInput of
        Left ex -> do
            return "Invalid input"
        Right genreName -> do
            let genre_fn = head (words genreName)
            let genre_sn = getLastElement (tail (words genreName))
            let sql1 = "SELECT DISTINCT g.* FROM genres g WHERE g.first_name = ? AND g.second_name = ?"
            genreResults <- query conn sql1 (genre_fn, genre_sn) :: IO [Genre]
            if not (null genreResults) then do
                let sql2 = "SELECT DISTINCT m.* FROM movies m JOIN movie_genre mg ON m.id == mg.fk_movie_genre JOIN genres g ON mg.fk_genre_genre == g.id WHERE g.first_name = ? AND g.second_name = ? ORDER BY m.year DESC"
                movieResults <- query conn sql2 (genre_fn, genre_sn) :: IO [Movie] 
                let g_fn = genre_first_name_ (head genreResults)
                let g_sn = genre_second_name_ (head genreResults)
                let num_movies = length movieResults
                let output = "Name: " ++ g_fn ++ " " ++ g_sn ++ "\nNo. of movies: " ++ show num_movies ++ "\nLatest movie: "
                if num_movies /= 0 then do
                    let latest_movie = title_ (head movieResults)
                    return (output ++ latest_movie)
                else 
                    return (output ++ "None") 
            else return "Error: this genre isn't in our database."

-- |The 'queryYear' allows the user to see details of a specfic year
queryYear :: Connection -> IO String
queryYear conn = do
    putStr "Enter year > "
    userInput <- try readLn :: IO (Either SomeException Int)
    case userInput of
        Left ex -> do
            return "Invalid input"
        Right year -> do
            let sql = "SELECT DISTINCT m.* FROM movies m WHERE m.year = ?" 
            movieResults <- query conn sql [year] :: IO [Movie]
            if not (null movieResults) then do
                let num_movies = length movieResults
                let latest_movie = title_ $ last movieResults
                return ("Year: " ++ show year ++ "\nNo. of movies released: " ++ show num_movies ++ "\nLatest movie: " ++ latest_movie)
            else return "Error: this year is not featured in our database"