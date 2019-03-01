{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import           Data.Aeson
import           Data.DocRecord
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           GHC.Generics
import           Porcupine.Run
import           Porcupine.Serials
import           Porcupine.Tasks
import qualified Streaming.Prelude           as S



-- This example uses the porcupine to read a data that represents the evloution of a given stock in given data and
-- gives back the average and standard deviation of the stock on that date.

data Stockdaily = Stockdaily { date :: String , high :: Double , low :: Double} deriving (Generic)
instance FromJSON Stockdaily

data Stock = Stock { chart :: [Stockdaily] }
  deriving (Generic)
instance FromJSON Stock

getHighStock :: Stock -> [Double]
getHighStock s = map high (chart s)

getLowStock :: Stock -> [Double]
getLowStock s = map low (chart s)

getDateStock :: Stock -> [String]
getDateStock s = map date (chart s)

-- | How to load Stock prices
stockFile :: DataSource Stock
stockFile = dataSource ["Inputs", "Stock"]
                      (somePureDeserial JSONSerial)


-- We do sliding windows for smothing the curve
data SlidingWindows = SlidingWindows { smoothcurve :: [Double] }
  deriving (Generic)
instance ToJSON SlidingWindows


-- | How to modify the data
modifiedStock :: DataSink SlidingWindows
modifiedStock = dataSink ["Outputs", "ModifiedStock"]
                        (somePureSerial JSONSerial)

ave :: [Double] -> Double
ave list = let s = sum list
               n = fromIntegral (length list)
               in s/n

msliding :: Int -> [a] -> [[a]]
msliding n p = case p of
  []     -> []
  (x:xs) -> [take n p] ++ (msliding n xs)


-- | The simple computation we want to perform
computeSmoothedCurve :: Stock -> SlidingWindows
computeSmoothedCurve s = SlidingWindows curve where
  price = getLowStock s
  curve = map ave (msliding 10 price)

-- | The task combining the three previous operations.
--
-- This task may look very opaque from the outside, having no parameters and no
-- return value. But we will be able to reuse it over different users without
-- having to change it at all.
analyseOneStock :: (LogThrow m) => PTask m () ()
analyseOneStock =
  loadData stockFile >>> arr computeSmoothedCurve >>> writeData modifiedStock

analyseStocks :: (LogThrow m) => PTask m () ()
analyseStocks =
  arr (const (S.each ["aapl" , "fb" , "googl"])) >>> loadDataStream "company" stockFile
   >>> arr (S.map (\(idx,stock) -> (idx, computeSmoothedCurve stock)))
   >>> writeDataStream "company" modifiedStock

main :: IO ()
main = runPipelineTask (FullConfig "example2" "porcupine-example2.yaml" "porcupine-core/examples/example2/data")
                          -- The CLI/Yaml configuration to use (prog name,
                          -- default config file to create, and default root to
                          -- use for the resource tree)
                       (baseContexts "")
                          -- The contexts to use. 'baseContexts' is the
                          -- minimum. It gives out katip logging and local files
                          -- access (through ResourceT). The string param is the
                          -- top namespace for the logger. When we use
                          -- FullConfig (and therefore CLI), the progName for
                          -- the CLI given above ("example1") will be inherited
                          -- by the logger, so we can leave it blank
                       analyseStocks ()
