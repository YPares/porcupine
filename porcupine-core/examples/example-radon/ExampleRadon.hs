{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE Arrows #-}

import           Control.Monad
import           Data.Aeson
import qualified Data.Csv as Csv
import           Data.DocRecord
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import Data.Functor
import           GHC.Generics
import           Porcupine.Run
import           Porcupine.Serials
import           Porcupine.Tasks
import           Prelude                       hiding (id, (.))
import Control.Lens
import qualified Control.Foldl as L
import           Graphics.Vega.VegaLite        as VL
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced
import Numeric.Log

import Plotting  -- In the same folder


data RadonObservation = RadonObservation
  { state :: !T.Text
  , county :: !T.Text
  , basement :: !T.Text
  , log_radon :: !Double }
  deriving (Generic, FromJSON, ToJSON
           ,Csv.FromNamedRecord, Csv.ToNamedRecord, Csv.DefaultOrdered)

-- | We want to read each RadonObservation as a set of Records. This supports
-- reading from CSV files with headers and from JSON files. The Vector cannot
-- directly be read from the CSV, as we would not known whether the columns are
-- positional or nominal. This is why we use the 'Records' wrapper here (for
-- nominal columns). This requires our datatype to instanciate
-- Csv.From/ToNamedRecord
radonObsSerials :: BidirSerials (V.Vector RadonObservation)
radonObsSerials = dimap Records fromRecords $  -- We wrap/unwrap the Records
  someBidirSerial (CSVSerial "csv" True ',')
  <>
  someBidirSerial JSONSerial

radonObsFile :: DataSource (V.Vector RadonObservation)
radonObsFile = dataSource ["data", "radon"] radonObsSerials

filteredCsvFile :: DataSink (V.Vector RadonObservation)
filteredCsvFile = dataSink ["debug", "radon-filtered"] radonObsSerials

vegaliteSerials :: PureSerials VegaLite
vegaliteSerials =
  lmap VL.toHtml (somePureSerial $ PlainTextSerial $ Just "html")
  <> lmap VL.fromVL (somePureSerial JSONSerial)

writeViz name = writeData (dataSink ["viz", name] vegaliteSerials)

data Summary = Summary { numRows :: Int
                       , uniqueStates :: [T.Text]
                       , numUniqueCounties :: Int }
  deriving (Show)

foldSummary :: L.Fold RadonObservation Summary
foldSummary = Summary <$> L.length
                      <*> L.premap state L.nub
                      <*> (L.premap county L.nub <&> length)

data ModelParams = ModelParams
  { rateWithB :: Double -- ^ ratio of houses with and without basement
  , radonWithB :: Double -- ^ radon level in houses with basement
  , radonWithoutB :: Double -- ^ radon level in houses without basement
  , noiseWithB :: Double -- ^ variation around radonWithB
  , noiseWithoutB :: Double -- ^ variation around radonWithoutB
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

priorModel :: MonadSample m => m ModelParams
priorModel =
    ModelParams <$> uniform 0 1
                <*> uniform 0 10
                <*> uniform 0 10
                <*> uniform 0 10
                <*> uniform 0 10

likelihood :: ModelParams -> (Bool, Double) -> Log Double
likelihood params (hasBasement, radonObserved) = 
    case hasBasement of
        True -> let radonModel = radonWithB params
                    noiseModel = noiseWithB params
                    rate = realToFrac $ rateWithB params
                in rate * normalPdf radonModel noiseModel radonObserved
        False -> let radonModel = radonWithoutB params
                     noiseModel = noiseWithoutB params
                     rate = realToFrac $ 1 - rateWithB params
                in rate * normalPdf radonModel noiseModel radonObserved

model :: MonadInfer m => [(Bool, Double)] -> m ModelParams
model observations = do
    params <- priorModel
    mapM_ (score . likelihood params) observations
    return params

posteriorForward :: MonadSample m => m ModelParams -> m (Bool, Double)
posteriorForward model = do
    params <- model
    hasBasement <- bernoulli (rateWithB params)
    value <- case hasBasement of
        True -> normal (radonWithB params) (noiseWithB params)
        False -> normal (radonWithoutB params) (noiseWithoutB params)
    return (hasBasement, value)

sampleFlatLinRegModel :: (LogThrow m) => PTask m () ()
sampleFlatLinRegModel = proc () -> do
  radonObs <- loadData radonObsFile -< ()
  writeData filteredCsvFile -< radonObs
  let (summary,xs,ys) = flip L.fold radonObs $
        (,,) <$> foldSummary
             <*> L.premap ((== "Y") . basement) L.list
             <*> L.premap log_radon L.list
      xLbl = "has basement"
      yLbl = "log radon"
  logInfo -< show summary
  
  vizSize <- getOption ["viz", "options"]
             (docField @"vizSize" (400,400) "(w,h) of visualisations") -< ()
  writeViz "1" -< plot vizSize
                       (S $ scatter2 xLbl yLbl (-3,5))
                       (Cols [(xLbl, VL.Booleans xs)
                             ,(yLbl, VL.Numbers ys)])
  nsamples <- getOption ["sampling", "options"]
              (docField @"nsamples" 5000 "Number of samples to draw") -< ()
  samples <- ioPTask -<
    sampleIOfixed $ prior $ mh nsamples $ model (zip xs ys)
  writeViz "2" -< plot vizSize
                       (H [[density2DPlot "radonWithB" "radonWithoutB" (0,2) (0,2)]
                          ,[density2DPlot "noiseWithB" "noiseWithoutB" (0,2) (0,2)]])
                       (J samples)

  samples <- ioPTask -<
    sampleIOfixed $ prior $ mh nsamples $ posteriorForward $ model (zip xs ys)
  let (xModel, yModel) = unzip samples
  writeViz "3" -< plot vizSize
    (S $ scatter2 xLbl yLbl (-3,5))
    (Cols [(xLbl, VL.Booleans xModel)
          ,(yLbl, VL.Numbers yModel)])
  

runIn topdir = runPipelineTask
  (FullConfig "example-radon"  -- Name of the executable (for --help)
              "example-radon.yaml" -- Default config file path
              topdir) -- Default root directory for mappings
  (baseContexts "")
  sampleFlatLinRegModel ()

main :: IO ()
main = runIn "examples/example-radon"
