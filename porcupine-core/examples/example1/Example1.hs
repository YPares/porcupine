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


data User = User { userName    :: T.Text
                 , userSurname :: T.Text
                 , userAge     :: Int }
  deriving (Generic)
instance FromJSON User

newtype Analysis = Analysis { numLetters :: HM.HashMap Char Int }
  deriving (Generic)
instance ToJSON Analysis

-- | How to load users
userFile :: DataSource User
userFile = dataSource ["Inputs", "User"]
                      (somePureDeserial JSONSerial)

-- | How to write analysis
analysisFile :: DataSink Analysis
analysisFile = dataSink ["Outputs", "Analysis"]
                        (somePureSerial JSONSerial)

-- | The simple computation we want to perform
computeAnalysis :: User -> Analysis
computeAnalysis (User name surname _) = Analysis $
  HM.fromListWith (+) $ [(c,1) | c <- T.unpack name]
                     ++ [(c,1) | c <- T.unpack surname]

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
  getOption ["Settings"] (docField @"users" (oneIndex (0::Int)) "The user ids to load")
  -- We turn the range we read into a full lazy list:
  >>> arr enumIndices
  -- Then we just map over these ids and call analyseOneUser each time:
  >>> parMapTask_ "userId" analyseOneUser

main :: IO ()
main = runPipelineTask (FullConfig "example1" "porcupine-example1.yaml" "porcupine-core/examples/example1/data")
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
