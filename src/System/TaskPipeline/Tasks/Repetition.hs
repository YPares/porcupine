{-# LANGUAGE OverloadedStrings #-}

module System.TaskPipeline.Tasks.Repetition
  ( repeatATask
  , repeatATask_
  ) where

import           Control.Lens                 (over, (^.), _1)
import           Control.Monad
import           Data.Aeson
import           Data.Locations
import qualified Data.Text                    as T
import           Katip
import           Prelude                      hiding ((.))
import qualified System.FilePath              as P
import           System.TaskPipeline.ATask
import           System.TaskPipeline.Resource


data TaskRepetitionContext = TRC
  { keyName      :: T.Text
  , repetitionId :: T.Text
  , verb         :: Verbosity }

instance ToJSON TaskRepetitionContext where
  toJSON (TRC k i _) = object [ k .= i ]
instance ToObject TaskRepetitionContext
instance LogItem TaskRepetitionContext where
  payloadKeys v (TRC _ _ v') | v >= v' = AllKeys
                             | otherwise = SomeKeys []

-- | Turns a task into something that will be repeated once per each item in its
-- input. BEWARE: This is done by transforming **EVERY** Loc mapped to every
-- leaf of the input tree, by appending it the identifier received in the task's
-- input list. The updated output tree of the task will be the one returned by
-- the **first** repetition.
repeatATask
  :: (Monad m, KatipContext m, Show identifier)
  => T.Text                        -- ^ A key for the logger context to indicate
                                   -- which repetition we're at
  -> Verbosity                     -- ^ The minimal vebosity level at which to
                                   -- display this logger context
  -> ATask m PipelineResource a b  -- ^ The base task X to repeat
  -> ATask m PipelineResource [(identifier, a)] [(identifier, b)]
                                   -- ^ A task that will repeat X it for each
                                   -- input. Each input is associated to a
                                   -- identifier that will be appended to
                                   -- every Loc mapped to every leaf in the
                                   -- LocationTree given to X.
repeatATask contextKey verb (ATask reqTree perform) = ATask reqTree perform'
  where
    perform' (inputs, origTree) = do
      (results, resultTrees) <- unzip <$> forM inputs
        (\(ident, inp) ->
           katipAddContext (TRC contextKey (T.pack $ show ident) verb) $ do
            (res, newTree) <- perform ( inp, updateTree ident origTree )
            return ((ident, res), newTree)
        )
      return (results, case resultTrees of
                         []  -> origTree
                         h:_ -> h)
    updateTree ident =
      over (everyLeaf . locTreeNodeTag . rscAccessed . pRscVirtualFile . locLayers . _1)
           (updateLoc ident)
    -- We change the filename of every loc bound to a leaf, to add the
    -- identifier to it
    updateLoc ident loc = dir </> (fname ++ "-" ++ show ident) <.> T.unpack ext
      where
        dir = takeDirectory loc
        fname = P.dropExtension $ P.takeFileName (loc ^. locPath)
        ext = loc ^. locExt

-- | See 'repeatATask'. Just ignores the result.
repeatATask_
  :: (Monad m, KatipContext m, Show identifier)
  => T.Text                        -- ^ A key for the logger context to indicate
                                   -- which repetition we're at
  -> Verbosity                     -- ^ The minimal vebosity level at which to
                                   -- display this logger context
  -> ATask m PipelineResource a b  -- ^ The base task X to repeat
  -> ATask m PipelineResource [(identifier, a)] ()
                                   -- ^ A task that will repeat X it for each
                                   -- input. Each input is associated to a
                                   -- identifier that will be appended to
                                   -- every Loc mapped to every leaf in the
                                   -- LocationTree given to X.
repeatATask_ k v t = repeatATask k v t >>> arr (const ())
