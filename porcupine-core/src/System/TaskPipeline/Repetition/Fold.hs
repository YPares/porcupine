{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module System.TaskPipeline.Repetition.Fold where

-- import           Control.Arrow
import           Control.Arrow.FoldA
-- import           Control.Category
import           Data.Locations
import           Prelude                               hiding (id, (.))
import           System.TaskPipeline.PTask
-- import           System.TaskPipeline.Repetition.Internal


-- * Folding data with a PTask

-- | Turns a fold in some monad to a fold compatible with 'foldTask'
unsafeGeneralizeM :: (KatipContext m)
                  => FoldM m a b -> FoldA (PTask m) a b
unsafeGeneralizeM (FoldM step start done) =
  FoldA (unsafeLiftToPTask $ \(Pair a x) -> step a x)
        (unsafeLiftToPTask $ const start)
        (unsafeLiftToPTask done)

-- data RepeatablePTask m a b =
--   RPT (LocationTree VirtualFile) (FoldA (RunnablePTask m) a b)

-- -- | Turns a 'FoldA' of PTask into something that can be safely repeated.
-- makeRepeatable
--   :: LocVariable
--   -> Maybe Verbosity
--   -> FoldA (PTask m) a b
--   -> RepeatablePTask m a b
-- makeRepeatable repetitionKey mbVerb =
--   where
--     (reqs, perform) = 

-- foldTask
--   :: (Foldable f)
--   => FoldA (PTask m) a b
--   -> PTask m (f a) b
-- foldTask = undefined
