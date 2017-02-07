{-# LANGUAGE DeriveGeneric #-}

module Airtable.Table
    ( RecordID(..)
    , rec2str
    , IsRecord(..)
    , Table(..)
    , TableName
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

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Data.Hashable


-- * RecordID 

-- | Airtable's record ID for use in indexing records
newtype RecordID = RecordID Text deriving (FromJSON, Show, Read, Eq, Generic, Ord)

instance Hashable RecordID 

rec2str :: RecordID -> String
rec2str (RecordID rec) = T.unpack rec

-- * IsRecord class

-- | Convenience typeclass for selecting record using RecordID-like values.
class IsRecord a where
  toRec :: a -> RecordID

instance IsRecord RecordID where
  toRec = id

instance IsRecord String where
  toRec = RecordID . T.pack 

-- * Table 

-- | Airtable's table type, a map for indexed lookup of records
data Table a = Table { tableRecords :: Map.HashMap RecordID a
                     , tableOffset :: Maybe Text
                     } deriving 
                     ( Show
                     , Read
                     )

-- | Used only to query API. 
type TableName = String

instance (FromJSON a) => FromJSON (Table a) where
  parseJSON (Object v) = do
    recs <- v .: "records" :: Parser [Value]
    parsedRecs <- foldlM parseRec Map.empty recs
    offset <- v .:? "offset"
    return $ Table parsedRecs offset
    where
      parseRec tbl (Object v) = 
            do  recId <- v .: "id"
                obj <- v .: "fields" 
                return $ Map.insert recId obj tbl
        <|> error ("could not decode: " <> show v)

instance Monoid (Table a) where
  mempty = Table mempty Nothing
  mappend (Table t1 o) (Table t2 _) = Table (mappend t1 t2) o

-- * Table methods

toList :: Table a -> [(RecordID, a)]
toList = Map.toList . tableRecords

exists :: (IsRecord r) => Table a -> r -> Bool
exists tbl rec = Map.member (toRec rec) (tableRecords tbl)

-- | Unsafely lookup a record using its RecordID.
select :: (HasCallStack, IsRecord r, Show a) => Table a -> r -> a
select tbl rec = tableRecords tbl `lookup` toRec rec
  where
    lookup mp k = case Map.lookup k mp of 
      Just v -> v
      Nothing -> error $ "lookup failed in map: " <> show k

-- | Safely lookup a record using its RecordID.
selectMaybe :: (IsRecord r, Show a) => Table a -> r -> Maybe a
selectMaybe tbl rec = toRec rec `Map.lookup` tableRecords tbl 

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
