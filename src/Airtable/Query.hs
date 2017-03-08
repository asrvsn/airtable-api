{-# LANGUAGE OverloadedStrings #-}

module Airtable.Query
    ( module Airtable.Table
      -- * Configuration for Airtable requests.
    , AirtableOptions(..)
    , defaultAirtableOptions
    , mkWreqOptions
    , tableNameToUrl
      -- * API
    , getRecords
    , getRecordsFromUrl
    , createRecord
    , updateRecord
    , deleteRecord
    -- * Low-level helpers
    , respJsonBody
    ) where


import           Airtable.Table

import           GHC.Stack
import           Network.Wreq

import           Control.Lens ((^.), (.~), (&))
import           Control.Monad (void)

import           Data.Aeson (FromJSON, ToJSON, eitherDecode, toJSON)
import           Data.Monoid
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BC

-- * Configuration for Airtable requests.

-- | Options
data AirtableOptions = AirtableOptions {
  -- | the API key for your project
    apiKey :: String
  -- | the app ID for your project (http:\/\/api.airtable.com\/v0\/app...)
  , appId :: String
  -- | api version (http:\/\/api.airtable.com\/v..\/...)
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

-- | Produce Wreq options from 'AirtableOptions'
mkWreqOptions :: AirtableOptions -> Options
mkWreqOptions opts = 
  defaults & header "Authorization" .~ ["Bearer " <> BC.pack (apiKey opts)]

-- | Produce a request URL to a table.
tableNameToUrl :: AirtableOptions -> TableName -> String
tableNameToUrl opts tname = 
     base_url
  <> "v"
  <> show (apiVersion opts)
  <> "/"
  <> appId opts
  <> "/"
  <> tname

-- * API

-- | Retrieve the records for a table from airtable.com given its name. Handles pagination correctly.
getRecords :: (HasCallStack, FromJSON a) => AirtableOptions -> TableName -> IO (Table a)
getRecords opts tname = 
  getRecordsFromUrl (mkWreqOptions opts) (tableNameToUrl opts tname)

-- | Retrieve the records for a table given a URL and network (Wreq) options.
getRecordsFromUrl :: (HasCallStack, FromJSON a) => Options -> String -> IO (Table a)
getRecordsFromUrl opts url = do
  resp <- getWith opts url
  getMore (respJsonBody resp)
  where
    getMore tbl = case tableOffset tbl of
      Just offset -> do
        resp <- getWith (opts & param "offset" .~ [offset]) url
        getMore $ respJsonBody resp <> tbl
      Nothing ->
        pure tbl

-- | Upload a record to a given table. Returns the newly created `RecordID`.
createRecord :: (HasCallStack, ToJSON a, FromJSON a) => AirtableOptions -> TableName -> a -> IO (Record a)
createRecord opts tname a = do
  resp <- postWith netOpts url payload
  return $ respJsonBody resp
  where
    url = tableNameToUrl opts tname
    netOpts = mkWreqOptions opts 
    payload = toJSON a

-- | Update a record on a given table, using the supplied fields ('a').
updateRecord :: (ToJSON a) => AirtableOptions -> TableName -> RecordID -> a -> IO ()
updateRecord opts tname recId a = do
  void $ 
    customPayloadMethodWith "PATCH" netOpts url payload
  where
    url = tableNameToUrl opts tname <> "/" <> rec2str recId
    netOpts = mkWreqOptions opts
    payload = toJSON a

-- | Delete a record on a table. 
deleteRecord :: AirtableOptions -> TableName -> RecordID -> IO ()
deleteRecord opts tname recId = 
  void $ deleteWith netOpts url 
  where
    netOpts = mkWreqOptions opts
    url = tableNameToUrl opts tname <> "/" <> rec2str recId

-- * Low-level helpers

respJsonBody :: (HasCallStack, FromJSON a) => Response ByteString -> a
respJsonBody r = decoder $ r ^. responseBody
  where
    decoder b = case eitherDecode b of
      Left e  -> error e
      Right r -> r
