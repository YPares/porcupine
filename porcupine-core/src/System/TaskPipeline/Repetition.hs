{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module System.TaskPipeline.Repetition
  ( RepInfo(..)
  , TRIndex(..)
  , HasTRIndex(..)
  , OneOrSeveral(..)
  , seqMapTask
  , seqMapTask_
  , parMapTask
  , parMapTask_
  , basicFoldlTask
  , filterTask
  , foldlA
  , IndexRange(..)
  , oneIndex
  , oneRange
  , enumIndices
  , enumTRIndices
  ) where

import           Control.Applicative
import           Control.Arrow.Free
import           Control.Lens                             hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types                        (Parser)
import qualified Data.Text                               as T
import           Prelude                                 hiding ((.))
import           System.TaskPipeline.PTask
import           System.TaskPipeline.Repetition.Internal


-- | Makes a 'PTask' repeatable and maps it sequentially over a list.
seqMapTask
  :: (HasTRIndex a, KatipContext m)
  => RepInfo  -- ^ With OverloadedStrings, you can just give the index name as a
              -- string litteral
  -> PTask m a b
  -> PTask m [a] [b]
seqMapTask ri =
  over taskRunnablePart mapSeqA . makeTaskRepeatable ri

-- | Simply repeats sequentially a task which takes no input over a list of
-- indices, and ignores the end result. See 'RepInfo' for how these indices are
-- used. See 'parMapTask' for a more complete version.
seqMapTask_
  :: (HasTRIndex idx, KatipContext m)
  => RepInfo  -- ^ With OverloadedStrings, you can just give the index name as a
              -- string litteral
  -> PTask m () b
  -> PTask m [idx] ()
seqMapTask_ ri task =
   arr (map (, ())) >>> seqMapTask ri (arr snd >>> task) >>> arr (const ())

-- | Makes a 'PTask' repeatable and maps it in parallel over a list.
parMapTask
  :: (HasTRIndex a, KatipContext m)
  => RepInfo
  -> PTask m a b
  -> PTask m [a] [b]
parMapTask ri =
  over taskRunnablePart mapA . makeTaskRepeatable ri

-- | Simply repeats in parallel a task which takes no input over a list of
-- indices, and ignores the end result. See 'RepInfo' for how these indices are
-- used. See 'seqMapTask' for a more complete version.
parMapTask_
  :: (HasTRIndex idx, KatipContext m)
  => RepInfo  -- ^ With OverloadedStrings, you can just give the index name as a
              -- string litteral
  -> PTask m () b
  -> PTask m [idx] ()
parMapTask_ ri task =
   arr (map (, ())) >>> parMapTask ri (arr snd >>> task) >>> arr (const ())

-- | Makes a 'PTask' repeatable and uses it to filter a list.
filterTask
  :: (HasTRIndex a, KatipContext m)
  => RepInfo  -- ^ With OverloadedStrings, you can just give the index name as a
              -- string litteral
  -> PTask m a Bool
  -> PTask m [a] [a]
filterTask ri =
  over taskRunnablePart filterA . makeTaskRepeatable ri

-- | Repeats an arrow step in order to fold a list
foldlA :: ArrowChoice a => a (b,acc) acc -> a ([b],acc) acc
foldlA f = proc (input,acc) ->
  case input of
    [] -> returnA -< acc
    (x:xs) -> do
      !acc' <- f -< (x,acc)
      foldlA f -< (xs,acc')

-- | Makes a 'PTask' repeatable and uses it to fold a list. For simple folds
-- only. For more complex foldings, use FoldA's.
basicFoldlTask
  :: (HasTRIndex b, KatipContext m)
  => RepInfo  -- ^ With OverloadedStrings, you can just give the index name as a
              -- string litteral
  -> PTask m (b,acc) acc  -- ^ The task used to return a new version of the
                          -- accumulator from current version and next input
                          -- from list
  -> PTask m ([b],acc) acc  -- ^ Task task that takes the list and the initial
                            -- version of the accumulator
basicFoldlTask ri =
  over taskRunnablePart foldlA . makeTaskRepeatable ri

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

-- | Gives a list of TaskRepetitionIndex
enumTRIndices :: (Enum i, Show i) => IndexRange i -> [TRIndex]
enumTRIndices = map (TRIndex . show) . enumIndices
