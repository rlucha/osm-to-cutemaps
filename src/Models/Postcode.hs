{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Postcode where

data Postcode = Postcode { lat :: Double, long :: Double }
  deriving (Eq, Show)