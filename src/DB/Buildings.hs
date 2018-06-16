module DB.Buildings where

import DB
import DB.Config as Config
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import Models.Building

getByPostcodeQuery :: String
getByPostcodeQuery = "select  \
  \ json_extract_path(ST_AsGeoJSON(way, 15)::json, 'coordinates', '0')::json as points \
  \ from planet_osm_polygon p \
  \ where ST_DWithin(p.way, ST_GeomFromText('SRID=3857;POINT(-7443.93434831 6699728.966326051)'), 1000) \
  \ AND building is not NULL"

getByPostcode :: String -> IO [Building]
getByPostcode pcs = do
  conn <- connectPostgreSQL Config.connString
  -- select <- prepare conn "SELECT * from postcodes_uk where postcode like 'SE228NP'"
  -- _ <- execute select [toSql pcs]
  results <- quickQuery conn getByPostcodeQuery []
  pure $ sqlToBuilding <$> results 
