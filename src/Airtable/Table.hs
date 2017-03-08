{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Airtable.Table
    (
    -- * Record
      Record(..)
    -- * RecordID
    , RecordID(..)
    , rec2str
    -- * HasRecordId class
    , HasRecordId(..)
    -- * Table
    , Table(..)
    , TableName
    , parseRecord
    , parseFields
    -- * Table methods
    , toList
    , exists
    , select
    , selectMaybe
    , selectAll
    , selectAllKeys
    , selectWhere
    , selectKeyWhere
    , deleteWhere
    ) where


import           GHC.Generics
import           GHC.Stack

import           Control.Applicative ((<|>))

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Data.Hashable
import           Data.Foldable (foldlM)
import           Data.Time (UTCTime)


-- * RecordID

-- | Airtable's record ID for use in indexing records
newtype RecordID = RecordID Text deriving ( FromJSON
                                          , ToJSON
                                          , Show
                                          , Read
                                          , Eq
                                          , Generic
                                          , Ord
                                          )

instance Hashable RecordID

rec2str :: RecordID -> String
rec2str (RecordID rec) = T.unpack rec

-- * HasRecordId class

-- | A convenience typeclass for selecting records using RecordID-like keys.
class HasRecordId a where
  toRecId :: a -> RecordID

instance HasRecordId RecordID where
  toRecId = id

instance HasRecordId String where
  toRecId = RecordID . T.pack

-- * Table

-- | An airtable record.
data Record a = Record 
  { recordId :: RecordID
  , recordObj :: a
  , createdTime :: UTCTime 
  } deriving ( Show
             , Read
             , Generic
             , Eq
             , Ord
             )

instance (FromJSON a) => FromJSON (Record a) where
  parseJSON = withObject "record object" $ \v -> do
    Record <$> v .: "id" 
           <*> v .: "fields"
           <*> v .: "createdTime"

instance (ToJSON a) => ToJSON (Record a) where
  toJSON rec = object [ "id" .= recordId rec
                      , "fields" .= recordObj rec
                      , "createdTime" .= createdTime rec
                      ]

instance HasRecordId (Record a) where
  toRecId = recordId

-- | Airtable's table type
data Table a = Table { tableRecords :: Map.HashMap RecordID a
                     , tableOffset :: Maybe Text
                     } deriving
                     ( Show
                     , Read
                     , Eq
                     , Generic
                     , Functor
                     )

-- | Synonym used in querying tables from the API.
type TableName = String

instance (FromJSON a) => FromJSON (Table a) where
  parseJSON = withObject "table object" $ \v -> do
    recs <- v .: "records" :: Parser [Value]
    parsedRecs <- foldlM parseRec Map.empty recs
    offset <- v .:? "offset"
    return $ Table parsedRecs offset
    where
      parseRec tbl = withObject "record object" $ \v -> 
            do  recId <- v .: "id"
                fields <- v .: "fields"
                obj <- parseJSON fields
                return $ Map.insert recId obj tbl

parseRecord :: (UTCTime -> RecordID -> Value -> Parser a) -> Value -> Parser a
parseRecord action = withObject "record object" $ \v -> do
    created  <- v .: "createdTime"
    recordId <- v .: "id"
    fields   <- v .: "fields"
    action created recordId fields

parseFields :: (Value -> Parser a) -> Value -> Parser a
parseFields action (Object v) = v .: "fields" >>= action
parseFields _action invalid = typeMismatch "parseFields_" invalid

instance Monoid (Table a) where
  mempty = Table mempty Nothing
  mappend (Table t1 o) (Table t2 _) = Table (mappend t1 t2) o

-- * Table methods

-- | Convert a 'Table' to a list of key-record pairs.
toList :: Table a -> [(RecordID, a)]
toList = Map.toList . tableRecords

-- | Check if a record exists at the given key in a table.
exists :: (HasRecordId r) => Table a -> r -> Bool
exists tbl rec = Map.member (toRecId rec) (tableRecords tbl)

-- | Unsafely lookup a record using its RecordID. Will throw a pretty-printed error
--   if record does not exist.
select :: (HasCallStack, HasRecordId r, Show a) => Table a -> r -> a
select tbl rec = tableRecords tbl `lookup` toRecId rec
  where
    lookup mp k = case Map.lookup k mp of
      Just v -> v
      Nothing -> error $ "lookup failed in map: " <> show k

-- | Safely lookup a record using its RecordID.
selectMaybe :: (HasRecordId r) => Table a -> r -> Maybe a
selectMaybe tbl rec = toRecId rec `Map.lookup` tableRecords tbl

-- | Read all records.
selectAll :: Table a -> [a]
selectAll = map snd . toList

-- | Read all RecordID's.
selectAllKeys :: Table a -> [RecordID]
selectAllKeys = map fst . toList

-- | Select all records satisfying a condition.
selectWhere :: Table a -> (RecordID -> a -> Bool) -> [a]
selectWhere tbl f = map snd $ filter (uncurry f) (toList tbl)

-- | Select all RecordID's satisfying a condition.
selectKeyWhere :: Table a -> (RecordID -> a -> Bool) -> [RecordID]
selectKeyWhere tbl f = map fst $ filter (uncurry f) (toList tbl)

-- | Delete all Records satisfying a condition.
deleteWhere :: Table a -> (RecordID -> a -> Bool) -> Table a
deleteWhere (Table recs off) f = Table (Map.filterWithKey (\k v -> not $ f k v) recs) off
