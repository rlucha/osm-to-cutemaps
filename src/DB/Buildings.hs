{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module DB.Buildings where

import Data.Maybe

import Text.Format
import DB.Config as Config
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import GHC.Generics
import Models.Building
import Data.Aeson 
import qualified Data.ByteString.Lazy.Char8 as B

data SqlCoords = SqlCoords [[Double]]
  deriving (Show, Generic)

instance FromJSON SqlCoords
instance ToJSON SqlCoords


getBuildingsQuery :: String -> String
getBuildingsQuery geom = format "select  \
  \ json_extract_path(ST_AsGeoJSON(way, 15)::json, 'coordinates', '0')::json as points \
  \ from planet_osm_polygon p \
  \ where ST_DWithin(p.way, ST_GeomFromText('SRID=3857;{0}'), 1000) \
  \ AND building is not NULL" [geom]

getByPointGeom :: String -> IO [Building]
getByPointGeom geom = do
  conn <- connectPostgreSQL Config.connString
  results <- quickQuery conn (getBuildingsQuery geom) []
  pure $ sqlToBuilding <$> results 

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
