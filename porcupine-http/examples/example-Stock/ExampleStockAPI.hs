{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

import           Data.Aeson
import           Data.DocRecord
import qualified Data.Text                     as T
import           GHC.Generics
import           Porcupine.Run
import           Porcupine.Serials
import           Porcupine.Tasks
-- import qualified Streaming.Prelude   as S

import           Prelude                       hiding (id, (.))

import           Data.Locations.Accessors.HTTP
import           Graphics.Vega.VegaLite




data StockDaily = StockDaily { date :: String , close :: Double} deriving (Generic)
instance FromJSON StockDaily
instance ToJSON StockDaily


data Stock = Stock { chart :: [StockDaily] } deriving (Generic)
instance FromJSON Stock
instance ToJSON Stock


getCloseStock :: Stock -> [Double]
getCloseStock s = map close (chart s)

getDateStock :: Stock -> [String]
getDateStock s = map date (chart s)

-- | How to load Stock prices
stockFile :: DataSource Stock
stockFile = dataSource ["Inputs", "Stock"]
                       (somePureDeserial JSONSerial)

-- As an example, we have donwloaded the Apple stock information from
-- https://api.iextrading.com/1.0/stock/aapl/batch?types=chart&range=1y
-- you can replace "aapl" by NASDAQ of any other company.


stockToVegaLite :: Stock -> VLSpec
stockToVegaLite stock =
  let dat = dataFromColumns []
          . dataColumn "Date" (Strings  (map T.pack (getDateStock stock) ) )
          . dataColumn "Price" (Numbers  (getCloseStock stock) )
      enc = encoding
          . position X [ PName "Date", PmType Temporal ]
          . position Y [ PName "Price", PmType Quantitative]
  in (fromVL . toVegaLite) [ dat [], width 800 , height 500 ,  mark Line [], enc [] ]

stockSmoothed :: DataSink Stock
stockSmoothed = dataSink ["Outputs", "StockSmoothed"]
                         (somePureSerial JSONSerial)

stockVegaLite :: DataSink VLSpec
stockVegaLite = dataSink ["Outputs", "StockSmoothedVegaLite"]
                         (somePureSerial JSONSerial)

-- We do sliding windows for smothing the curve


ave :: [Double] -> Double
ave list = let s = sum list
               n = fromIntegral (length list)
               in s/n

msliding :: Int -> [a] -> [[a]]
msliding n p = case p of
  []     -> []
  (_:xs) -> [take n p] ++ (msliding n xs)

-- | The simple computation we want to perform
computeSmoothedCurve :: Stock -> Stock
computeSmoothedCurve stock = Stock { chart = [ StockDaily { date = d , close = p} | (d,p) <- datePriceZipped ] } where
  price = getCloseStock stock
  priceSmoothed = map ave (msliding 1 price)
  datePriceZipped = zip (getDateStock stock) priceSmoothed

analyseStock :: (LogThrow m) => PTask m () ()
analyseStock =
   loadData stockFile
   >>> arr computeSmoothedCurve
   >>> (proc s -> do
          writeData stockSmoothed -< s
          writeData stockVegaLite -< stockToVegaLite s)
   -- >>> arr (\s -> (s , stockToVegaLite s ))
   -- >>> (writeData stockSmoothed *** writeData stockVegaLite)
   -- >>> arr (const ())

newtype IdCompany = IdCompany String
  deriving (Generic)
instance Show IdCompany where
  show (IdCompany s) = s
instance ToJSON IdCompany
instance FromJSON IdCompany

mainTask :: (LogThrow m) => PTask m () ()
mainTask =
  getOption ["Settings"]
    (docField @"idcompany" [IdCompany "aapl"] "The NASDAQ of the company to load")
  >>> parMapTask_ (repIndex "idcompany") analyseStock


-- globalMatrix :: DataSink (Tabular [[Double]])
-- globalMatrix = dataSink [ "Outputs" , "globalData"]
--                         (somePureSerial (CSVSerial (T.pack "csv") False ','))
--
-- putallStocks :: [SlidingWindows] -> Tabular [[Double]]
-- putallStocks s = Tabular Nothing (map smoothcurve s)
--
-- analyseStocks :: (LogThrow m) => PTask m () ()
-- analyseStocks =
--   arr (const (S.each ["aapl" , "fb" , "googl"])) >>> loadDataStream "company" stockFile
--    >>> arr (S.map (\(idx,stock) -> (idx, computeSmoothedCurve stock)))
--    >>> unsafeLiftToPTask (S.toList_)
--    >>> arr (map snd)
--    >>> arr putallStocks
--    >>> writeData globalMatrix

main :: IO ()
main = runPipelineTask (FullConfig "example-stock" "porcupine-http/examples/example-Stock/example-stock.yaml" "porcupine-http/examples/example-Stock/data")
                       (#http <-- useHTTP :& baseContexts "")
                       mainTask ()
