{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DB.Common where

import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Models.Building (Building(..), Coords(..))
import Models.Postcode (Postcode(..))
import Data.Text

import DB.Config
import DB.Postcodes
import DB.Buildings


-- Check System.Environment
-- Check Control.Exception & bracket
-- Check ReaderT design Pattern

-- this path is absolute to the root of the project it seems
-- get postcode from the environment or non checked-in file
-- connString = "host=localhost dbname=osm_london port=3306 user='postgres' password='5jDnpuFFN21pF10n'"

-- getPostcode :: String -> IO Postcode
-- getPostcode pcs = do
--   conn <- connectPostgreSQL connString
--   results <- quickQuery conn "SELECT * from postcodes where postcode = ?" [toSql pcs]
--   -- This should be a Maybe / Either and handle those cases properly
--   pure $ sqlToPostcode (head results)

-- Check precision changes with JS code in Gist
degrees2meters :: Double ->  Double -> (Double, Double)
degrees2meters lat' long' = (x', y')
  where x' = long' * 20037508.34 / 180.00
        y' = (log(tan((90 + lat') * pi / 360.00)) / (pi / 180.00)) * 20037508.34 / 180.00

postcode2Mercator :: Postcode -> (Double, Double)
postcode2Mercator (Postcode _ long' lat') = degrees2meters long' lat'

showBuildings :: String -> IO [Building]
showBuildings ps = do
  postcode  <- DB.Postcodes.getPostcode $ (unpack . toUpper . pack) ps
  let (lat', long') = postcode2Mercator postcode
      geom = "POINT(" ++ show lat' ++ " " ++ show long' ++ ")"
  print geom
  DB.Buildings.getByPointGeom geom
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