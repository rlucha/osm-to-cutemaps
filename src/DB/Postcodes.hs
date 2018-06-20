module DB.Postcodes where

import qualified DB.Config as Config
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import Models.Postcode

getPostcodeQuery :: String
getPostcodeQuery = "select * from ons_postcodes where postcode = ?"

getPostcode :: String -> IO Postcode
getPostcode pc = do
  conn <- connectPostgreSQL Config.connString
  select <- prepare conn getPostcodeQuery
  _ <- execute select [toSql pc]
  result <- fetchAllRows select
  pure $ sqlToPostcode $ head result

sqlToPostcode :: [SqlValue] -> Postcode
sqlToPostcode row = Postcode 
  { postcode = fromSql $ row!!0
  , lat = fromSql $ row!!1 
  , long = fromSql $ row!!2 
  }
