module System.TaskPipeline.Repetition.Internal where

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
makeRepeatable
  :: (Show i, Monad m)
  => LocVariable
  -> Maybe Verbosity
  -> PTask m a b
  -> PTask m (i,a) b
makeRepeatable repetitionKey mbVerb =
  over splittedPTask $ \(reqTree, runnable) ->
    ( fmap addKeyToVirtualFile reqTree
    , modifyingRuntimeState alterState snd runnable )
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
