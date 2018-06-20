{-# LANGUAGE OverloadedStrings #-}

module Routes.Buildings where

import Web.Scotty
import Control.Monad.IO.Class

import DB.Common

getByPostcode :: ActionM ()
getByPostcode = do
  postcode <- param "postcode"
  room <- liftIO (DB.Common.showBuildings postcode) -- liftIO to actionM
  json room