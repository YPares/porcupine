{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC "-fno-warn-missing-signatures" #-}

module Test.Porcupine.PTask where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Internal.Property (unGroupName, unPropertyName)

import Control.Monad.Trans
import Control.Arrow
import Control.Category
import Control.Lens
import Data.String
import Prelude hiding (id, (.))
import Katip

import System.TaskPipeline.PTask
import System.TaskPipeline.PTask.Internal
import System.TaskPipeline.Run (simpleRunPTask)
import System.TaskPipeline.Logger


taskResultIs task inp f =
  liftIO (simpleRunPTask task inp) >>= f

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

hunitTests :: [TestTree]
hunitTests =
  [ testGroup "RunnableTasks basic functions"
    [ --runnableResultIs (withRunnableState $ const )
    ]
  , testGroup "Tasks/katip integration"
    [ testCase "Namespace is transmitted correctly" $
      runnableResultIs
        (view taskRunnablePart $ addNamespaceToTask "test-ns" $
           runnableWithoutReqs $
           withRunnableState $ \st _ ->
             return $ st ^. ptrsKatipNamespace)
        mempty () (@=? (Namespace ["main", "test-ns"]))
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
