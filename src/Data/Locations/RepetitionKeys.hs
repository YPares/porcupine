{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Locations.RepetitionKeys where

import           Control.Lens                hiding ((<.>))
import           Data.Hashable               (Hashable)
import qualified Data.HashMap.Strict         as HM
import           Data.List                   (intercalate)
import           Data.Locations.Loc
import           Data.Locations.LogAndErrors
import           Data.Monoid
import           Data.String                 (IsString)
import qualified Data.Text                   as T
import qualified System.FilePath             as P


-- | Some 'VirtualFile's can be accessed in tasks that will be part of a
-- 'repeatPTask' loop: it means these files should "occur" several times (for
-- instance "file-0.json", "file-1.json" etc.). Each time, their location should
-- be altered (eg. suffixed) by the content of a value associated to some
-- key(s), so that we don't repeatedly read or overwrite the same file.
newtype RepetitionKey = RepetitionKey T.Text
  deriving (Hashable, Eq, Read, IsString)

instance Show RepetitionKey where
  show (RepetitionKey t) = T.unpack t

-- | 'RepetitionKey's associated with their current values.
type RepetitionKeyMap = HM.HashMap RepetitionKey T.Text

-- | A reordered 'RepetitionKeyMap'
type RepetitionKeyValList = [(RepetitionKey, Maybe T.Text)]

-- | Adds the values of the repetition keys to the loc filename.
spliceRKeysInLoc :: [(RepetitionKey, T.Text)] -> LocWithVars -> LocWithVars
spliceRKeysInLoc rkeys = spliceLocVariables hm
  where
    hm = HM.fromList $ map (\(RepetitionKey k, v) -> (T.unpack k, T.unpack v)) rkeys

-- | Fills the missing values in the 'RepetitionKeyValList' with the content of
-- the 'RepetitionKeyMap'
fillRKeyValList :: RepetitionKeyValList -> RepetitionKeyMap -> RepetitionKeyValList
fillRKeyValList required keyMap = map findOne required
  where findOne (k, v) = (k, getFirst $ First v <> First (HM.lookup k keyMap))

-- | Throws errors if not all the keys are associated to a value
terminateRKeyValList :: (MonadThrow m, KatipContext m)
                     => RepetitionKeyValList -> m [(RepetitionKey, T.Text)]
terminateRKeyValList rkeys = do
  let split (k, Just v) (ok, notOk)  = ((k,v):ok, notOk)
      split (k, Nothing) (ok, notOk) = (ok, k:notOk)
      (rkeys', keysWithoutVal) = foldr split ([],[]) rkeys
  case keysWithoutVal of
    [] -> return rkeys'
    _  -> throwWithPrefix $ "simpleWriteToLocFn: Repetition keys "
          ++ show keysWithoutVal ++ " don't have associated values"
