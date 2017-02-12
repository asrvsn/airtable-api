{-# LANGUAGE OverloadedStrings #-}

module Airtable.Query
    ( module Airtable.Table
      -- * Configuration for Airtable requests.
    , AirtableOptions(..)
    , defaultAirtableOptions
      -- * Main API
    , getTable
    ) where


import           Airtable.Table

import           Network.Wreq

import           Control.Lens ((^.), (.~), (&))

import           Data.Aeson (FromJSON, ToJSON, eitherDecode)
import           Data.Monoid
import qualified Data.ByteString.Char8 as BC

-- | Options
data AirtableOptions = AirtableOptions {
  -- | the API key for your project
    apiKey :: String
  -- | the app ID for your project (http://api.airtable.com/v0/...)
  , appId :: String
  -- | api version (http://api.airtable.com/v../...)
  , apiVersion :: Int
  }

-- | Airtable options defaulting to API version 0. Please change the
--   'apiKey' and 'appId' fields.
defaultAirtableOptions :: AirtableOptions
defaultAirtableOptions = AirtableOptions {
    apiKey = ""
  , appId = ""
  , apiVersion = 0
  }

-- | Base airtable API string.
base_url :: String
base_url = "https://api.airtable.com/"

-- | Retrieve a table from airtable.com given its name. Handles pagination correctly.
getTable :: (FromJSON a) => AirtableOptions -> TableName -> IO (Table a)
getTable opts tname = getTableFromUrl net_otps url
  where
    net_otps = defaults & header "Authorization" .~ ["Bearer " <> BC.pack (apiKey opts)]
    url  =   base_url
          <> "v"
          <> show (apiVersion opts)
          <> "/"
          <> appId opts
          <> "/"
          <> tname

getTableFromUrl :: (FromJSON a) => Options -> String -> IO (Table a)
getTableFromUrl opts url = do
  resp <- getWith opts url
  getMore (fromResp resp)
  where
    getMore tbl = case tableOffset tbl of
      Just offset -> do
        resp <- getWith (opts & param "offset" .~ [offset]) url
        getMore $ fromResp resp <> tbl
      Nothing ->
        pure tbl

    fromResp r = decoder $ r ^. responseBody
      where
        decoder b = case eitherDecode b of
          Left e -> error $ e <> "\nSource string: " <> show b
          Right r -> r
