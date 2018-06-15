{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module DB where

import Data.Aeson 
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Models.Building (Building(..), Coords(..))
import Models.Postcode (Postcode(..))
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

data SqlCoords = SqlCoords [[Double]]
  deriving (Show, Generic)

instance FromJSON SqlCoords
instance ToJSON SqlCoords

-- Check System.Environment
-- Check Control.Exception & bracket
-- Check ReaderT design Pattern

-- this path is absolute to the root of the project it seems
-- get postcode from the environment or non checked-in file
connString = "host=localhost dbname=osm_london port=3306 user='postgres' password='5jDnpuFFN21pF10n'"

getBuildingsQuery = "select  \
  \ json_extract_path(ST_AsGeoJSON(way, 15)::json, 'coordinates', '0')::json as points \
  \ from planet_osm_polygon p \
  \ where ST_DWithin(p.way, ST_GeomFromText('SRID=3857;POINT(-7443.93434831 6699728.966326051)'), 1000) \
  \ AND building is not NULL"

getPostcodeBuildings :: String -> IO [Building]
getPostcodeBuildings pcs = do
  conn <- connectPostgreSQL connString
  -- select <- prepare conn "SELECT * from postcodes_uk where postcode like 'SE228NP'"
  -- _ <- execute select [toSql pcs]
  results <- quickQuery conn getBuildingsQuery []
  pure $ sqlToBuilding <$> results 

getPostcodePosition :: String -> IO Postcode
getPostcodePosition pcs = do
  conn <- connectPostgreSQL connString
  results <- quickQuery conn "SELECT * from postcodes where postcode like ?" [toSql pcs]
  -- This should be a Maybe / Either and handle those cases properly
  pure $ sqlToPostcode (head results)

sqlToBuilding :: [SqlValue] -> Building
sqlToBuilding row = building
  -- we know row is always going to be a single value (but better use maybe head here)
  where coords = (fromSql $ head row) :: String -- Coords.create get fst and snd from row
        -- prepareCoords = createCoord coords
        -- actually parse coords to a datatype (it is now a json string "[x,y]")
        -- Maybe use HDBA/Aseon approach and try to find how to convert this into the proper text representation
        stringCoords = (decode $ B.pack coords) :: Maybe SqlCoords
        parsedCoords = fromMaybe (SqlCoords [[]]) stringCoords
        createCoords (SqlCoords r) = (\[x',z'] -> Coords { x = x', y = 0, z = z'}) <$> r
        building = Building $ createCoords parsedCoords

sqlToPostcode :: [SqlValue] -> Postcode
sqlToPostcode row = Postcode { lat = fromSql $ row!!1, long = fromSql $ row!!2}

-- Check precision changes with JS code in Gist
degrees2meters :: Double ->  Double -> (Double, Double)
degrees2meters lat' long' = (x', y')
  where x' = long' * 20037508.34 / 180.00
        y' = (log(tan((90 + lat') * pi / 360.00)) / (pi / 180.00)) * 20037508.34 / 180.00

postcode2Mercator :: Postcode -> (Double, Double)
postcode2Mercator (Postcode long' lat') = degrees2meters long' lat'

showBuildings :: IO ()
showBuildings = do
  ps <- getPostcodePosition "SE228NP"
  print ps
  -- bs <- getPostcodeBuildings "SE228NP"
  print $ postcode2Mercator ps

  -- The Latitude, Longitude of se228np is:
  -- 51.44130,-0.06687  -- 

  
-- get1 :: [SqlValue] -> String
-- get1 row = fromSql $ row!!1
  
  -- sqlToBuilding :: ([SqlValue], [[SqlValue]]) -> Building
  -- sqlToBuilding (row, exits') = Room roomID' name' title' description' mapExits
  --   where roomID' = fromSql $ head row
  --         name' = fromSql $ row!!1
  --         title' = fromSql $ row!!2
  --         description' = fromSql $ row!!3
  --         mapExits = catMaybes $ sqlToExit <$> exits'
      
  
  -- {-# LANGUAGE OverloadedStrings #-}
  -- {-# LANGUAGE DeriveGeneric #-}
  --
  -- module Db where
  --
  -- import GHC.Generics
  -- import Database.HDBC
  -- import Database.HDBC.Sqlite3
  -- import Data.Aeson (FromJSON, ToJSON)
  --
  -- import Room
  --
  -- data GameUser = GameUser Integer String String
  --   deriving (Show, Generic)
  --
  -- instance ToJSON GameUser
  -- instance FromJSON GameUser
  --
  -- sqlToGameUser :: [SqlValue] -> GameUser
  -- sqlToGameUser row = GameUser (fromSql $ head row) (fromSql $ row!!1) (fromSql $ row!!2)
  --
  -- getAllUsers :: IO [GameUser]
  -- getAllUsers = do
  --   -- this path is absolute to the root of the project it seems
  --   conn <- connectSqlite3 "db.sql"
  --   q <- quickQuery' conn "select * from users" []
  --   return (map sqlToGameUser q)
  --
  -- getUser :: Integer -> IO GameUser
  -- getUser id = do
  --   conn <- connectSqlite3 "db.sql"
  --   q <- quickQuery' conn "select * from users where id = ?" [toSql id]
  --   return $ head (map sqlToGameUser q)
  
  -- THIS WORKS
  -- conn <- connectPostgreSQL connString
  -- results <- quickQuery conn getBuildingsQuery []
  -- a = head results -- Why is 'a' an array of SqlByteString?
  -- b = head a
  -- c = fromSql b :: String
  -- d =(decode $ B.pack c) :: Maybe SqlCoords
  -- e = fromMaybe (SqlCoords [[]]) d
  -- createCoords (SqlCoords r) = (\[x',z'] -> Coords { x = x', y = 0, z = z'}) <$> r
  -- f = Building $ createCoords e
  -- TODO toJSON Building, return [Building] as JSON to Scotty