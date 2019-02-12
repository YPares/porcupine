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



-- This example uses the porcupine to read a data that represents the evloution of a given stock in given data and
-- gives back the average and standard deviation of the stock on that date.


data Stock = Stock { stockName    :: T.Text
                 , stockprice :: [Float] }
  deriving (Generic)
instance FromJSON Stock

data Analysis = Analysis { ave :: Float
                            , std :: Float  }
  deriving (Generic)
instance ToJSON Analysis

-- | How to load Stock prices
userFile :: DataSource Stock
userFile = dataSource ["Inputs", "Stock"]
                      (somePureDeserial JSONSerial)

-- | How to write analysis
analysisFile :: DataSink Analysis
analysisFile = dataSink ["Outputs", "Analysis"]
                        (somePureSerial JSONSerial)

-- | The simple computation we want to perform
computeAnalysis :: Stock -> Analysis
computeAnalysis (Stock name price) = Analysis ave std where
  ave =
     let s = sum price
         n = fromIntegral (length price)
     in s/n
  std =
    let s = sum [ (c- ave)^2 | c <- price]
        n = fromIntegral (length price)
    in sqrt (s/n)

-- | The task combining the three previous operations.
--
-- This task may look very opaque from the outside, having no parameters and no
-- return value. But we will be able to reuse it over different users without
-- having to change it at all.
analyseOneUser :: (LogThrow m) => PTask m () ()
analyseOneUser =
  loadData userFile >>> arr computeAnalysis >>> writeData analysisFile

mainTask :: (LogThrow m) => PTask m () ()
mainTask =
  -- First we get the ids of the users that we want to analyse. We need only one
  -- field that will contain a range of values, see IndexRange. By default, this
  -- range contains just one value, zero.
  getOption ["Settings"] (docField @"stocks" (oneIndex (0::Int)) "The stock ids to load")
  -- We turn the range we read into a full lazy list:
  >>> arr enumIndices
  -- Then we just map over these ids and call analyseOneUser each time:
  >>> parMapTask_ (repIndex "id") analyseOneUser

main :: IO ()
main = runPipelineTask (FullConfig "example2" "porcupine.yaml" "porcupine-core/examples/example2/data")
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
                       mainTask ()
