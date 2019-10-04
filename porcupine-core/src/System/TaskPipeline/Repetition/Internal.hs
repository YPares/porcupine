{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module System.TaskPipeline.Repetition.Internal
  ( RepInfo(..)
  , TRIndex(..)
  , HasTRIndex(..)
  , makeRepeatable
  ) where

import           Control.Category
import           Control.Lens                       hiding ((:>))
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict                as HM
import           Data.Locations
import           Data.String                        (IsString(..))
import           Katip
import           Prelude                            hiding (id, (.))
import           System.TaskPipeline.PorcupineTree
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal


-- | Gives information about how a task will be repeated. The repInfoIndex will
-- be used as a suffix in the default bindings to locations accessed by this
-- task. If repInfoLogging is not Nothing, repInfoIndex will also be mentioned
-- in the context of each line logged by the task to identify which repetition
-- of the task is generating this log line. RepInfo is an instance of IsString
-- so you can use it with OverloadedStrings (in which case repInfoIndex will be
-- added to the context when verbosity level is at least 1).
data RepInfo = RepInfo
  { repInfoIndex   :: LocVariable
  -- ^ A name that will be used as a metavariable in the config file. It may
  -- also be used by the logger as a context key, to indicate which repetition
  -- is currently running.
  , repInfoLogging :: Maybe Verbosity
  -- ^ The minimal vebosity level at which to display the value associated with
  -- the repetition index in the logger context. Nothing if we don't want to add
  -- context.
  } deriving (Eq, Show)

instance IsString RepInfo where
  fromString s = RepInfo (fromString s) (Just V1)

-- | Logging context for repeated tasks
data TaskRepetitionContext = TRC
  { _repetitionKey     :: LocVariable
  , _repetitionKeyVal  :: String
  , _repetitionKeyVerb :: Verbosity }

instance ToJSON TaskRepetitionContext where
  toJSON (TRC k v _) = toJSON $ HM.singleton k v
instance ToObject TaskRepetitionContext
instance LogItem TaskRepetitionContext where
  payloadKeys v (TRC _ _ v') | v >= v' = AllKeys
                             | otherwise = SomeKeys []

-- | Task Repetition Index. Is given to functions that repeat tasks for each
-- iteration.
newtype TRIndex = TRIndex { unTRIndex :: String }
  deriving (FromJSON, ToJSON)

instance IsString TRIndex where
  fromString = TRIndex

-- | The class of every data that can be repeated
class HasTRIndex a where
  getTRIndex :: a -> TRIndex

instance HasTRIndex TRIndex where
  getTRIndex = id

instance HasTRIndex Int where
  getTRIndex = TRIndex . show

instance HasTRIndex Integer where
  getTRIndex = TRIndex . show

instance (HasTRIndex i) => HasTRIndex (i,a) where
  getTRIndex (i,_) = getTRIndex i

-- | Turns a task into one that can be called several times, each time with a
-- different index value @i@. This index will be used to alter every path
-- accessed by the task. The first argument gives a name to that index, that
-- will appear in the configuration file in the default bindings for the
-- VirtualFiles accessed by this task. The second one controls whether we want
-- to add to the logging context which repetition is currently running.
makeRepeatable
  :: (HasTRIndex a, KatipContext m)
  => RepInfo
  -> PTask m a b
  -> PTask m a b
makeRepeatable (RepInfo repetitionKey mbVerb) =
  over splittedPTask
    (\(reqTree, runnable) ->
      ( fmap addKeyToVirtualFile reqTree
      , modifyingRuntimeState alterState id runnable ))
  where
    addKeyToVirtualFile VirtualFileNode{..} =
      VirtualFileNode
      {vfnodeFile = vfnodeFile &
        over (vfileSerials.serialRepetitionKeys) (repetitionKey:)
      ,..}
    addKeyToVirtualFile emptyNode = emptyNode

    alterState input =
        over ptrsKatipContext alterContext
      . over (ptrsDataAccessTree.traversed) addKeyValToDataAccess
      where
        idxStr = unTRIndex $ getTRIndex input
        newCtxItem = TRC repetitionKey idxStr <$> mbVerb
        alterContext ctx = case newCtxItem of
          Nothing   -> ctx
          Just item -> ctx <> liftPayload item
        addKeyValToDataAccess (DataAccessNode l fn) =
          DataAccessNode l $ fn . HM.insert repetitionKey idxStr
        addKeyValToDataAccess emptyNode = emptyNode
