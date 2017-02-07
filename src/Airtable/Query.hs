{-# LANGUAGE OverloadedStrings #-}

module Airtable.Query
    ( module Airtable.Table
    , AirtableOptions
    , getTable
    ) where


import           Airtable.Table

import           Network.Wreq

import           Control.Lens ((^.), (.~), (&))

import           Data.Aeson (FromJSON, ToJSON, eitherDecode)
import           Data.Monoid
import qualified Data.ByteString.Char8 as BC

-- * Configuration for Airtable requests.

data AirtableOptions = AirtableOptions {
    apiKey :: String
  , appId :: String  
  }

-- * Constants

-- | Base airtable API string. 
base_url :: String
base_url = "https://api.airtable.com/v0/"

-- * Main API

-- | Retrieve a table from airtable.com given its name. 
getTable :: (FromJSON a) => AirtableOptions -> TableName -> IO (Table a)
getTable opts tname = getTableFromUrl net_otps url 
  where
    net_otps = defaults & header "Authorization" .~ ["Bearer " <> BC.pack (apiKey opts)] 
    url  = base_url <> appId opts <> "/" <> tname

getTableFromUrl :: (FromJSON a) => Options -> String -> IO (Table a)
getTableFromUrl opts url = do
  resp <- getWith opts url
  getMore (fromResp resp)
  where
    getMore tbl = case tableOffset tbl of 
      Just offset -> do
        resp <- getWith (opts & param "offset" .~ [offset]) url
        putStrLn $ "Part " <> show offset
        getMore $ fromResp resp <> tbl
      Nothing -> 
        pure tbl

    fromResp r = decoder $ r ^. responseBody
      where
        decoder b = case eitherDecode b of 
          Left e -> error $ e <> "\nSource string: " <> show b
          Right r -> r