{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module System.TaskPipeline.Repetition
  ( module System.TaskPipeline.Repetition.Streaming
  , RepInfo(..), repIndex
  , HasTaskRepetitionIndex(..)
  , OneOrSeveral(..)
  , parMapTask
  , parMapTask_
  , IndexRange(..)
  , oneIndex
  , oneRange
  , enumIndices
  ) where

import           Control.Applicative
import           Control.Arrow.Free                       (mapA)
import           Control.Lens                             hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import qualified Data.Text                                as T
import           Prelude                                  hiding ((.))
import           System.TaskPipeline.PTask
import           System.TaskPipeline.Repetition.Internal
import           System.TaskPipeline.Repetition.Streaming


-- | Makes a 'PTask' repeatable and maps it in parallel over a list.
parMapTask
  :: (HasTaskRepetitionIndex a, Monad m)
  => RepInfo
  -> PTask m a b
  -> PTask m [a] [b]
parMapTask ri =
  over ptaskRunnablePart mapA . makeRepeatable ri

-- | Simply repeats a task which takes no input over a list of indices, and
-- ignores the end result. See 'RepInfo' for how these indices are
-- used. See 'parMapTask' for a more complete version.
parMapTask_
  :: (Show idx, Monad m)
  => RepInfo
  -> PTask m () b
  -> PTask m [idx] ()
parMapTask_ ri task =
   arr (map (, ())) >>> parMapTask ri (arr snd >>> task) >>> arr (const ())


-- * A simple type to handle index ranges

data OneRange i = OneIndex i | OneRange i i

toJSONStr :: (ToJSON a) => a -> Either Value T.Text
toJSONStr a = case toJSON a of
  String s -> Right s
  Number n -> Right $ T.pack $ show n
  o        -> Left o

parseJSONStr :: (FromJSON a) => T.Text -> Parser a
parseJSONStr v = tryNumber v <|> parseJSON (String v)
  where
    tryNumber n = case reads $ T.unpack n of
      [(n',_)] -> parseJSON $ Number n'
      _        -> fail "Not a number"

instance (ToJSON i) => ToJSON (OneRange i) where
  toJSON (OneIndex i) = toJSON i
  toJSON (OneRange a b) = case (toJSONStr a, toJSONStr b) of
    (Right a', Right b') -> String $ a' <> ".." <> b'
    (a', b')             -> object ["lower" .= toJ a', "upper" .= toJ b']
    where toJ (Left o)  = o
          toJ (Right s) = String s

instance (FromJSON i) => FromJSON (OneRange i) where
  parseJSON o@(String s) = case T.splitOn ".." s of
    [a,b] -> (OneRange <$> parseJSONStr a <*> parseJSONStr b)
             <|> (OneIndex <$> parseJSON o)
    _ -> OneIndex <$> parseJSON o
  parseJSON (Object o) = OneRange <$> o .: "lower" <*> o .: "upper"
  parseJSON o = OneIndex <$> parseJSON o

-- | Allows to read from a JSON file either one @a@ or an array of @a@
newtype OneOrSeveral a = OneOrSeveral {getOneOrSeveral :: [a]}

instance (ToJSON a) => ToJSON (OneOrSeveral a) where
  toJSON (OneOrSeveral [r]) = toJSON r
  toJSON (OneOrSeveral rs)  = toJSON rs

instance (FromJSON a) => FromJSON (OneOrSeveral a) where
  parseJSON o@(Array _) = OneOrSeveral <$> parseJSON o
  parseJSON o           = OneOrSeveral . (:[]) <$> parseJSON o

-- | A simple index list that can be used in configuration, and from which a
-- list of indices can be extracted. The JSON representation of it is more
-- compact than that of [(i,i)], as ranges are represented by "a..b" strings
newtype IndexRange i = IndexRange (OneOrSeveral (OneRange i))
  deriving (FromJSON, ToJSON)

-- | A range of just one index
oneIndex :: i -> IndexRange i
oneIndex i = IndexRange $ OneOrSeveral [OneIndex i]

-- | A range of consecutive values
oneRange :: i -> i -> IndexRange i
oneRange a b = IndexRange $ OneOrSeveral [OneRange a b]

-- | Gives a list of indices from an index range
enumIndices :: (Enum i) => IndexRange i -> [i]
enumIndices (IndexRange (OneOrSeveral rs)) = concatMap toL rs
  where
    toL (OneIndex i)   = [i]
    toL (OneRange a b) = [a..b]
