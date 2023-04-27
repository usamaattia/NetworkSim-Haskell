{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Types
Description : This module includes all the type definitions required throughout the application
-}
module Types (
    -- * Types
    Actor (..),
    Cast (..),
    Genre (..),
    MovieGenre (..),
    Movie (..),
    Record (..),
    Records (..)
) where

import GHC.Generics
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Data.Aeson

-- | This is the data structure that will represent an Actor entry
data Actor = Actor {
    -- | The 'actor_id' method returns the id by which an actor entry is indexed
    actor_id_ :: Int,
    -- | The 'actor_first_name_' method returns the first name of an actor entry
    actor_first_name_ :: String,
    -- | The 'actor_last_name_' method returns the last name of an actor entry
    actor_last_name_ :: String
} deriving (Show)

-- | A Cast entry associates an actor with a movie
data Cast = Cast {
    -- | The 'fk_movie_cast' method returns the id of the movie in this relation
    fk_movie_cast :: Int,
    -- | The 'fk_actor_cast' method returns the id of the actor in this relation
    fk_actor_cast :: Int
} deriving (Show)

-- | This is the data structure that will represent a Genre entry
data Genre = Genre {
    -- | The 'genre_id_' method returns the id by which a genre entry is indexed
    genre_id_ :: Int,
    -- | The 'genre_first_name_' method returns the first name of a genre entry
    genre_first_name_ :: String,
    -- | The 'genre_second_name_' method returns the second name of a genre entry
    genre_second_name_ :: String
} deriving (Show)

-- | A MovieGenre entry associates a genre with a movie
data MovieGenre = MovieGenre {
    -- | The 'fk_movie_genre' method returns the id of the movie in this MovieGenre relation
    fk_movie_genre :: Int,
    -- | The 'fk_genre_genre' method returns the id of the genre in this MovieGenre relation
    fk_genre_genre :: Int
} deriving (Show)

-- | This is the data structure that will represent a Movie entry
data Movie = Movie {
    -- | The 'movie_id_' method returns the id by which a movie entry is indexed
    movie_id_ :: Int,
    -- | The 'title_' method returns the title of a movie entry
    title_ :: String,
    -- | The 'year_' method returns the year of a movie entry
    year_ :: Int
} deriving (Show)

-- | This is the data structure that will represent a raw Record object
data Record = Record {
    -- | The 'title' method returns the title present in a given json record
    title :: String,
    -- | The 'year' method returns the year present in a given json record
    year :: Int,
    -- | The 'cast' method returns the list of actors present in a given json record
    cast :: [String], 
    -- | The 'genres' method returns the list of genres present in a given json record
    genres :: [String] 
} deriving (Show, Generic)

-- | 'Records' defines an alias for an array of Record's
newtype Records = Records {
    -- | The 'records' method returns the list of records
    records :: [Record]
} deriving (Show, Generic)


{-- Making above datatype instances of FromRow and ToRow type classes --}
-- | Defining Actor as an instance of FromRow
instance FromRow Actor where
    fromRow = Actor <$> field <*> field <*> field

-- | Defining Actor as an instance of ToRow
instance ToRow Actor where
    toRow (Actor i fname lname) = toRow (i, fname, lname)

-- | Defining Cast as an instance of FromRow
instance FromRow Cast where
    fromRow = Cast <$> field <*> field

-- | Defining Cast as an instance of ToRow
instance ToRow Cast where
    toRow (Cast mi ai) = toRow (mi, ai)

-- | Defining Genre as an instance of FromRow
instance FromRow Genre where
    fromRow = Genre <$> field <*> field <*> field

-- | Defining Genre as an instance of ToRow
instance ToRow Genre where
    toRow (Genre i fname lname) = toRow (i, fname, lname)

-- | Defining MovieGenre as an instance of FromRow
instance FromRow MovieGenre where
    fromRow = MovieGenre <$> field <*> field

-- | Defining MovieGenre as an instance of ToRow
instance ToRow MovieGenre where
    toRow (MovieGenre mi gi) = toRow (mi, gi)

-- | Defining Movie as an instance of FromRow
instance FromRow Movie where
    fromRow = Movie <$> field <*> field <*> field

-- | Defining Movie as an instance of ToRow
instance ToRow Movie where
    toRow (Movie i tit year) = toRow (i, tit, year)

{-|
Making above datatype instances of FromJSON type class
-}
-- |The 'renameFields' function is required to define the 'customOptions' function
renameFields :: String -> String
renameFields other = other

-- |The 'customOptions' function is required to define the 'parseJSON' function
customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

-- | Defining Record as an instance of FromJSON
instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

-- | Defining Records as an instance of FromJSON
instance FromJSON Records
