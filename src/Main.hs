{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Routes.Buildings

routes :: ScottyM ()
routes = get "/buildings/:postcode" Routes.Buildings.getByPostcode

main :: IO ()
main = scotty 3000 routes