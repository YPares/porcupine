{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Locations.RepetitionKeys where

import           Data.Hashable               (Hashable)
import qualified Data.HashMap.Strict         as HM
import           Data.String                 (IsString)


-- | Some 'VirtualFile's can be accessed in tasks that will be part of a
-- 'repeatPTask' loop: it means these files should "occur" several times (for
-- instance "file-0.json", "file-1.json" etc.). Each time, their location should
-- be altered (eg. suffixed) by the content of a value associated to some
-- key(s), so that we don't repeatedly read or overwrite the same file.
newtype RepetitionKey = RepetitionKey String
  deriving (Hashable, Eq, Read, IsString)

instance Show RepetitionKey where
  show (RepetitionKey t) = t

-- | 'RepetitionKey's associated with their current values.
type RepetitionKeyMap = HM.HashMap RepetitionKey String
