{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StaticPointers             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wall                   #-}

module System.RunContext
  ( RunContext(..)
  , curRoot, curModelName, curRunName, verbosity
  , HasRunContext, runContext, withAnnot
  , runWithContext, runWithContextAndRefLoc
  , inputDirName, outputDirName
  , dataDir, inputDir, outputDir
  , onInputDir, onOutputDir
  , PId(..), HasPatientId, patientId, withPId
  -- * Logging
  , Log.Verbosity(..), System.RunContext.log
  -- * Reexported
  , module Control.Monad.Reader
  , With(With), elt, ann
  )
  where

import           Control.Distributed.Closure    (Dict (..), Serializable,
                                                 Static, closureDict)
import           Control.Distributed.Closure.TH (withStatic)
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson

import           Data.Binary                    (Binary)
import           Data.Default                   (Default, def)
import           Data.Text                      (Text)
import           Data.Typeable                  (Typeable)
import           GHC.Generics                   (Generic)

import qualified Control.Logger                 as Log
import           Data.Locations.Loc             hiding ((</>))
import           Data.Locations.LocationMonad
import           Streaming.NovaUtils
import           System.FilePath                ((</>))


-- | A context for automatically finding artifacts paths during exploration
data RunContext =
  RunContext {
      _curRoot      :: Loc
    , _curModelName :: String
    , _curRunName   :: String
    , _verbosity    :: Log.Verbosity
  } deriving (Typeable, Generic, Show)

instance Default RunContext where
  def = RunContext
          { _curRoot = LocalFile "data"
          , _curModelName = "XXX"
          , _curRunName = "BaseLayer"
          , _verbosity = Log.Normal
          }

makeLenses ''RunContext

class HasRunContext a where
  runContext :: Lens' a RunContext

inputDirName :: String
inputDirName = "Inputs"
outputDirName :: String
outputDirName = "Outputs"

dataDir, inputDir, outputDir :: HasRunContext r => r -> FilePath
dataDir ctx = ctx^.runContext.curModelName </> ctx^.runContext.curRunName
inputDir ctx = dataDir ctx </> inputDirName
outputDir ctx = dataDir ctx </> outputDirName

onInputDir, onOutputDir :: HasRunContext r => FilePath -> r -> FilePath
onInputDir path ctx = inputDir ctx </> path
onOutputDir path ctx = outputDir ctx </> path

withStatic [d|
  instance HasRunContext RunContext where runContext = id

  instance HasRunContext r => HasRunContext (r `With` ann) where
    runContext = elt . runContext
  |]

instance Binary RunContext
instance Static (Serializable RunContext) where
  closureDict = static (Dict :: Dict (Serializable RunContext))

-- | A patient Id
newtype PId = PId { getPId :: Int }
  deriving (Eq, ToJSON, FromJSON, Num, Enum)

instance Show PId where
  show = show . getPId

-- | Used to find a patient Id ('PId') from possibly more global context
class HasPatientId a where
  patientId :: Lens' a PId

withStatic [d|
  instance HasPatientId PId where patientId = id

  instance HasPatientId annot => HasPatientId (r `With` annot) where
    patientId = ann.patientId
  |]

withPId :: (MonadReader env m, HasRunContext env)
        => ReaderT (env `With` PId) m a
        -> PId
        -> m a
withPId = withAnnot

withAnnot :: MonadReader env m
          => ReaderT (env `With` annot) m a
          -> annot
          -> m a
withAnnot runner annot =  do
  ctx <- ask
  runReaderT runner (With annot ctx)

log
  :: ( MonadReader env m
     , HasRunContext env
     , MonadIO m)
  => Log.Verbosity
  -> Text
  -> m ()
log v msg = do
  currentVerb <- view (runContext.verbosity) <$> ask
  Log.log currentVerb v msg

-- | Like 'Loc.selectRun' but allows shipping an extra context to the embeded
-- @MonadReader@. Depending on the root present in the context it determines
-- whether the action should run in IO (locally) or in AWS.
runWithContext
  :: (HasRunContext ctx)
  => ctx
  -> (forall m. (LocationMonadReader ctx m) => m a)
  -> IO a
runWithContext ctx action =
  runWithContextAndRefLoc (ctx^.runContext.curRoot) ctx action

-- | Depending on the Loc given, runs either locally or in AWS Monad (see
-- 'Loc.selectRun')
runWithContextAndRefLoc
  :: (HasRunContext ctx)
  => Loc
  -> ctx
  -> (forall m. (LocationMonadReader ctx m) => m a)
  -> IO a
runWithContextAndRefLoc refLoc ctx action =
  selectRun refLoc (ctx^.runContext.verbosity > Log.Silent) $
    runReaderT action ctx
