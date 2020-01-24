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
    -- * Table methods
    , fromRecords
    , fromList
    , toList
    , exists
    , select
    , vSelect
    , selectMaybe
    , vSelectMaybe
    , selectAll
    , vSelectAll
    , selectAllKeys
    , selectWhere
    , vSelectWhere
    , selectKeyWhere
    , vSelectKeyWhere
    , deleteWhere
    , vDeleteWhere
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
import           Data.Semigroup ((<>), Semigroup)
import           Data.Monoid hiding ((<>))
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
             , Functor
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

-- | An airtable table. 
data Table a = Table 
  { tableRecords :: Map.HashMap RecordID (Record a) -- ^ A mapping from RecordID to Record
  , tableOffset :: Maybe Text -- ^ The "offset" parameter for the table. Is used to deal with airtable's pagination.
  } deriving ( Show
             , Read
             , Eq
             , Generic
             , Functor
             )

-- | Synonym used in querying tables from the API.
type TableName = String

instance (FromJSON a) => FromJSON (Table a) where
  parseJSON = withObject "table object" $ \v -> do
    recs <- v .: "records" 
    offset <- v .:? "offset"
    return $ (fromRecords recs) { tableOffset = offset }

instance (ToJSON a) => ToJSON (Table a) where
  toJSON tbl = object [ "offset" .= tableOffset tbl
                      , "records" .= selectAll tbl
                      ]

instance Semigroup (Table a) where
  (Table t1 o) <> (Table t2 _) = Table (t1 <> t2) o

instance Monoid (Table a) where
  mempty = Table mempty Nothing
  mappend (Table t1 o) (Table t2 _) = Table (mappend t1 t2) o

-- * Table methods

-- | Create a `Table` from a list of `Record`s.
fromRecords :: [Record a] -> Table a
fromRecords recs = fromList $ zip (map recordId recs) recs

-- | Create a table from a list of key-record pairs.
fromList :: [(RecordID, Record a)] -> Table a
fromList pairs = Table
  { tableRecords = Map.fromList pairs 
  , tableOffset = Nothing
  }

-- | Convert a 'Table' to a list of key-record pairs.
toList :: Table a -> [(RecordID, Record a)]
toList = Map.toList . tableRecords

-- | Check if a record exists at the given key in a table.
exists :: (HasRecordId r) => Table a -> r -> Bool
exists tbl rec = Map.member (toRecId rec) (tableRecords tbl)

-- | Unsafely lookup a record using its RecordID. Will throw a pretty-printed error
--   if record does not exist.
select :: (HasCallStack, HasRecordId r, Show a) => Table a -> r -> Record a
select tbl rec = tableRecords tbl `lookup` toRecId rec
  where
    lookup mp k = case Map.lookup k mp of
      Just v -> v
      Nothing -> error $ "lookup failed in map: " <> show k

-- | Same as 'select', but returns the record object.
vSelect :: (HasCallStack, HasRecordId r, Show a) => Table a -> r -> a
vSelect tbl rec = recordObj (select tbl rec)

-- | Safely lookup a record using its RecordID.
selectMaybe :: (HasRecordId r) => Table a -> r -> Maybe (Record a)
selectMaybe tbl rec = toRecId rec `Map.lookup` tableRecords tbl

-- | Same as 'selectMaybe', but returns the record object.
vSelectMaybe :: (HasRecordId r) => Table a -> r -> Maybe a
vSelectMaybe tbl rec = recordObj <$> selectMaybe tbl rec

-- | Read all records.
selectAll :: Table a -> [Record a]
selectAll = map snd . toList

-- | Same as 'selectAll', but returns the record object.
vSelectAll :: Table a -> [a]
vSelectAll = map recordObj . selectAll

-- | Read all RecordID's.
selectAllKeys :: Table a -> [RecordID]
selectAllKeys = map fst . toList

-- | Select all records satisfying a condition.
selectWhere :: Table a -> (Record a -> Bool) -> [Record a]
selectWhere tbl f = filter f (selectAll tbl)

-- | Same as 'selectWhere', but returns the record object.
vSelectWhere :: Table a -> (a -> Bool) -> [a]
vSelectWhere tbl f = filter f (vSelectAll tbl)

-- | Select all records satisfying a condition.
selectKeyWhere :: Table a -> (Record a -> Bool) -> [RecordID]
selectKeyWhere tbl f = map recordId (selectWhere tbl f)

-- | Same as 'selectKeyWhere', but returns the record object.
vSelectKeyWhere :: Table a -> (a -> Bool) -> [RecordID]
vSelectKeyWhere tbl f = selectKeyWhere tbl (f . recordObj) 

-- | Delete all Records satisfying a condition.
deleteWhere :: Table a -> (Record a -> Bool) -> Table a
deleteWhere (Table recs off) f = Table (Map.filter (\v -> not $ f v) recs) off

-- | Same as 'deleteWhere', but returns the record object.
vDeleteWhere :: Table a -> (a -> Bool) -> Table a
vDeleteWhere tbl f = deleteWhere tbl (f . recordObj)
