{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Postcode where

data Postcode = Postcode { postcode :: String, lat :: Double, long :: Double }
  deriving (Eq, Show)