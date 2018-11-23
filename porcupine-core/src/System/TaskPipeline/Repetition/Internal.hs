module System.TaskPipeline.Repetition.Internal
  ( TaskRepetitionContext(..)
  , RepInfo(..)
  , withRepKey
  , makeRepeatable
  ) where

import           Control.Category
import           Control.Lens                          hiding ((:>), (.=))
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict                   as HM
import           Data.Locations
import           Katip
import           Prelude                               hiding (id, (.))
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.ResourceTree


-- | Gives information about how a task will be repeated
data RepInfo = RepInfo
  { repKey        :: LocVariable
  -- ^ A variable name, used as a key to indicate which repetition we're
  -- at. Used in the logger context and exposed in the yaml file for each
  -- VirtualFile that will be repeated by this task
  , repKeyLogging :: Maybe Verbosity
  -- ^ The minimal vebosity level at which to display the value associated with
  -- the repetition key in the logger context. Nothing if we don't want to add
  -- context.
  } deriving (Eq, Show)

-- | Creates a 'RepetitionInfo' that will log the repetition key at verbosity
-- level 1 and above.
withRepKey :: LocVariable -> RepInfo
withRepKey lv = RepInfo lv (Just V1)

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


-- | Turns a task into one that can be called several times, each time with a
-- different index value @i@. This index will be used to alter every path
-- accessed by the task. The first argument gives a name to that index, that
-- will appear in the configuration file in the default bindings for the
-- VirtualFiles accessed by this task. The second one controls whether we want
-- to add to the logging context which repetition is currently running.
--
-- The index is just passed through, to facilitate composition.
makeRepeatable
  :: (Show idx, Monad m)
  => RepInfo
  -> PTask m a b
  -> PTask m (idx,a) (idx,b)
makeRepeatable (RepInfo repetitionKey mbVerb) =
  over splittedPTask $ \(reqTree, runnable) ->
    ( fmap addKeyToVirtualFile reqTree
    , keepingIndex $ modifyingRuntimeState alterState snd runnable )
  where
    addKeyToVirtualFile (VirtualFileNode vf) =
      VirtualFileNode $ vf &
        over (vfileSerials.serialsRepetitionKeys) (repetitionKey:)
    addKeyToVirtualFile emptyNode = emptyNode

    alterState (idx,_) =
        over ptrsKatipContext alterContext
      . over (ptrsDataAccessTree.traversed) addKeyValToDataAccess
      where
        idxStr = show idx
        newCtxItem = TRC repetitionKey idxStr <$> mbVerb
        alterContext ctx = case newCtxItem of
          Nothing   -> ctx
          Just item -> ctx <> liftPayload item
        addKeyValToDataAccess (DataAccessNode l fn) =
          DataAccessNode l $ fn . HM.insert repetitionKey idxStr
        addKeyValToDataAccess emptyNode = emptyNode

    keepingIndex t =
      id &&& t >>> arr (\((idx,_),o) -> (idx, o))
