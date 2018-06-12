module DB where

import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Models.Building (Building(..), Coords(..))
import Models.Postcode (Postcode(..))

getPostcodeBuildings :: String -> IO [Building]
getPostcodeBuildings pcs = do
  -- this path is absolute to the root of the project it seems
  -- get postcode from the environment or non checked-in file
  conn <- connectPostgreSQL "host=localhost dbname=cute_london port=3306 user='postgres' password=''"
  -- select <- prepare conn "SELECT * from postcodes_uk where postcode like 'SE228NP'"
  -- _ <- execute select [toSql pcs]
  results <- quickQuery conn "SELECT * from postcodes where postcode like ?" [toSql pcs]
  -- results <- fetchAllRows select 
  pure $ sqlToBuilding <$> results 

getPostcodePosition :: String -> IO Postcode
getPostcodePosition pcs = do
  conn <- connectPostgreSQL "host=localhost dbname=cute_london port=3306 user='postgres' password=''"
  results <- quickQuery conn "SELECT * from postcodes where postcode like ?" [toSql pcs]
  -- This should be a Maybe / Either and handle those cases properly
  pure $ sqlToPostcode (head results)
   
sqlToBuilding :: [SqlValue] -> Building
sqlToBuilding row = Building { coordinates = [coords] }
  where coords = Coords { x = fromSql $ row!!1, y = 0, z = fromSql $ row!!2}

sqlToPostcode :: [SqlValue] -> Postcode
sqlToPostcode row = Postcode { lat = fromSql $ row!!1, long = fromSql $ row!!2}

-- Check precision changes with JS code in Gist
degrees2meters :: Double ->  Double -> (Double, Double)
degrees2meters long' lat' = (x', y')
  where x' = long' * 20037508.34 / 180.00
        y' = (log(tan((90 + lat') * pi / 360.00)) / (pi / 180.00)) * 20037508.34 / 180.00

postcode2Mercator :: Postcode -> (Double, Double)
postcode2Mercator (Postcode long' lat') = degrees2meters long' lat'

showBuildings :: IO ()
showBuildings = do
  ps <- getPostcodePosition "SE228NP"
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
  