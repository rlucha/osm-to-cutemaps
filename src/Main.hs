{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import DB
import Control.Monad.IO.Class

getBuildings :: ActionM ()
getBuildings = do
  postcode <- param "postcode"
  room <- liftIO (getPostcodeBuildings postcode) -- liftIO to actionM
  json room

routes :: ScottyM ()
routes = get "/buildings/:postcode" getBuildings

main :: IO ()
main = scotty 3000 routes