module Routes.Buildings where

import Web.Scotty
import Control.Monad.IO.Class

import DB.Buildings

getByPostcode :: ActionM ()
getByPostcode = do
  postcode <- param "postcode"
  room <- liftIO (DB.Buildings.getByPostcode postcode) -- liftIO to actionM
  json room