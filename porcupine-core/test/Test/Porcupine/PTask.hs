{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC "-fno-warn-missing-signatures" #-}

module Test.Porcupine.PTask where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property (unGroupName, unPropertyName)

import Control.Monad.IO.Class
import Control.Category
import Control.Lens
import Data.Typeable
import Prelude hiding (id, (.))
import Katip

import Data.Locations.LocationTree
import System.TaskPipeline.PTask
import System.TaskPipeline.PTask.Internal
import System.TaskPipeline.PorcupineTree
import System.TaskPipeline.Run (simpleRunPTask)
import System.TaskPipeline.Logger


taskResultIs task inp f =
  liftIO (simpleRunPTask task inp) >>= f

runnableResultIs :: (MonadIO m)
                 => RunnableTask (KatipContextT IO) a a1
                 -> DataAccessTree (KatipContextT IO)
                 -> a
                 -> (a1 -> m b)
                 -> m b
runnableResultIs task dataTree inp f =
  liftIO (
    runLogger "execRunnableTask" warningsAndErrorsLoggerScribeParams $
      execRunnableTask ffopts dataTree task inp ) >>= f
  where ffopts = FunflowOpts "/tmp/_funflow_tests/store"
                 "/tmp/_funflow_tests/coord.db" Nothing Nothing

prop_runnable_id :: Property
prop_runnable_id = property $ do
  x <- forAll $ Gen.int Range.linearBounded
  runnableResultIs id mempty x (=== x)

pureReadDataAccessNode :: forall a. (Typeable a) => a -> DataAccessNode (KatipContextT IO)
pureReadDataAccessNode x =
  DataAccessNode [] (const $ DataAccessor @(KatipContextT IO) @a @a
                             (const $ return ()) (return x) (Right []))

hunitTests :: [TestTree]
hunitTests =
  [ testGroup "RunnableTasks basic functions"
    []
  , testGroup "Tasks/katip integration"
    [ testCase "Namespace is transmitted correctly" $
      runnableResultIs
        (view taskRunnablePart $ addNamespaceToTask "test-ns" $
           runnableWithoutReqs $
           withRunnableState $ \st _ ->
             return $ st ^. ptrsKatipNamespace)
        mempty () (@=? (Namespace ["main", "test-ns"]))
    ]
  , testGroup "Modifying task context" $
    [ testGroup "taskInSubtree" $
      let subpath = ["a","b","c"]
          (reqs,runnable) = view splitTask $ taskInSubtree subpath $
            makeTask mempty $ \root () -> case _locTreeNodeTag root of
              DataAccessNode _ accessFn -> daPerformRead (accessFn mempty) >>= return . cast
              _ -> fail "Root node is empty"
          mkTree n =
            folderNode ["a" :/ folderNode ["b" :/ folderNode ["c" :/ locNode n []]]]
          da = pureReadDataAccessNode (10::Int)
      in
        [ testCase "Result" $
          runnableResultIs runnable (mkTree da) () (@=? (Just (10::Int)))
        , testCase "Requirements" $
          (const () <$> reqs) @=? mkTree ()
        ]
    ]
  ]

tests :: TestTree
tests = testGroup (unGroupName (groupName hhGroup))
  [ testGroup "Hedgehog" [ testProperty (unPropertyName n) p
                         | (n,p) <- groupProperties hhGroup ]
  , testGroup "HUnit" hunitTests
  ]
  where
    hhGroup = $$(discover)
