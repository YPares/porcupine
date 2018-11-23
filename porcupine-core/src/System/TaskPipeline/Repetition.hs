{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}

module System.TaskPipeline.Repetition
  ( module System.TaskPipeline.Repetition.Streaming
  , RepInfo(..), repIndex
  , parMapTask
  , parMapTask_
  , IndexRange(..)
  , oneIndex
  , oneRange
  , enumIndices
  ) where

import Control.Applicative
import Control.Arrow.Free (mapA)
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import System.TaskPipeline.PTask
import System.TaskPipeline.Repetition.Internal
import System.TaskPipeline.Repetition.Streaming
import Prelude hiding (id, (.))


-- | Makes a 'PTask' repeatable and maps it in parallel over a list.
parMapTask
  :: (Show idx, Monad m)
  => RepInfo
  -> PTask m a b
  -> PTask m [(idx,a)] [(idx,b)]
parMapTask ri =
  over ptaskRunnable mapA . makeRepeatable ri

-- | Simply repeats a task which takes no input over a list of indices, and
-- ignores the end result. See 'RepInfo' for how these indices are
-- used. See 'parMapTask' for a more complete version.
parMapTask_
  :: (Show idx, Monad m)
  => RepInfo
  -> PTask m () b
  -> PTask m [idx] ()
parMapTask_ ri task =
   arr (map (, ())) >>> parMapTask ri task >>> arr (const ())


-- * A simple type to handle index ranges

data OneRange i = OneIndex i | OneRange i i

toJSONStr :: (ToJSON a) => a -> Either Value T.Text
toJSONStr a = case toJSON a of
  String s -> Right s
  Number n -> Right $ T.pack $ show n
  o -> Left o

parseJSONStr :: (FromJSON a) => T.Text -> Parser a
parseJSONStr v = tryNumber v <|> parseJSON (String v)
  where
    tryNumber n = case reads $ T.unpack n of
      [(n',_)] -> parseJSON $ Number n'
      _ -> fail "Not a number"

instance (ToJSON i) => ToJSON (OneRange i) where
  toJSON (OneIndex i) = toJSON i
  toJSON (OneRange a b) = case (toJSONStr a, toJSONStr b) of
    (Right a', Right b') -> String $ a' <> ".." <> b'
    (a', b') -> object ["lower" .= toJ a', "upper" .= toJ b']
    where toJ (Left o) = o
          toJ (Right s) = String s

instance (FromJSON i) => FromJSON (OneRange i) where
  parseJSON o@(String s) = case T.splitOn ".." s of
    [a,b] -> (OneRange <$> parseJSONStr a <*> parseJSONStr b)
             <|> (OneIndex <$> parseJSON o)
    _ -> OneIndex <$> parseJSON o
  parseJSON (Object o) = OneRange <$> o .: "lower" <*> o .: "upper"
  parseJSON o = OneIndex <$> parseJSON o

-- | A simple index list that can be used in configuration, and from which a
-- list of indices can be extracted. The JSON representation of it is more
-- compact than that of [(i,i)], as ranges are represented by "a..b" strings
newtype IndexRange i = IndexRange [OneRange i]
  
-- | A range of just one index
oneIndex :: i -> IndexRange i
oneIndex i = IndexRange [OneIndex i]

-- | A range of consecutive values
oneRange :: i -> i -> IndexRange i
oneRange a b = IndexRange [OneRange a b]

instance (ToJSON i) => ToJSON (IndexRange i) where
  toJSON (IndexRange [r]) = toJSON r
  toJSON (IndexRange rs) = toJSON rs

instance (FromJSON i) => FromJSON (IndexRange i) where
  parseJSON o@(Array _) = IndexRange <$> parseJSON o
  parseJSON o = IndexRange . (:[]) <$> parseJSON o

-- | Gives a list of indices from an index range
enumIndices :: (Enum i) => IndexRange i -> [i]
enumIndices (IndexRange rs) = concatMap toL rs
  where
    toL (OneIndex i) = [i]
    toL (OneRange a b) = [a..b]
