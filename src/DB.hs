module DB where

import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Models.Building (Building(..), Coords(..))

getPostcodeBuildings :: String -> IO [Building]

getPostcodeBuildings pcs = do
  -- this path is absolute to the root of the project it seems
  conn <- connectPostgreSQL "host=localhost dbname=postcodes_pure3 user='' password=''"
  -- select <- prepare conn "SELECT * from postcodes_uk where postcode like 'SE228NP'"
  -- _ <- execute select [toSql pcs]
  results <- quickQuery conn "SELECT * from postcodes_uk where postcode like ?" [toSql pcs]
  -- results <- fetchAllRows select 
  pure $ sqlToBuilding <$> results 
   
sqlToBuilding :: [SqlValue] -> Building
sqlToBuilding row = Building { coordinates = [coords] }
  where coords = Coords { x = fromSql $ row!!2, y = 0, z = fromSql $ row!!3}
 
showBuildings :: IO [()]
showBuildings = do
  bs <- getPostcodeBuildings "SE228NP"
  traverse print bs



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
  