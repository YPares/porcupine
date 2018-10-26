{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

import Porcupine.Serials
import Porcupine.Tasks
import Porcupine.Run
import Data.DocRecord
import Data.Aeson
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.Generics


data User = User { userName :: T.Text
                 , userSurname :: T.Text
                 , userAge :: Int }
  deriving (Generic)
instance FromJSON User

data Analysis = Analysis { numLetters :: HM.HashMap Char Int }
  deriving (Generic)
instance ToJSON Analysis

userFile :: DataSource User
userFile = dataSource ["Inputs", "User"]
                      (somePureDeserial JSONSerial)

analysisFile :: DataSink Analysis
analysisFile = dataSink ["Outputs", "Analysis"]
                        (somePureSerial JSONSerial)

analyseOneUser :: (LocationMonad m, KatipContext m) => PTask m () ()
analyseOneUser =
  loadLast userFile >>> arr computeAnalysis >>> writeData analysisFile

mainTask :: (LocationMonad m, KatipContext m) => PTask m () ()
mainTask =
  -- First we get the ids of the users that we want to analyse (we need only one
  -- field that will contain the list of the ids):
  getOption ["Settings"] (docField @"userIds" [0::Int] "The user ids to load") >>>
  arr (map (, ())) >>>  -- The input of analyseOneUser is (), so we need a
                        -- stream of (Int, ()) pairs
  listToStreamTask >>>  -- Just a utility if you don't want to explicitly import Streaming
  -- Finally, we just loop over these ids and call analyseOneUser each time:
  mappingOverStream_ "userId" Nothing
    analyseOneUser

computeAnalysis :: User -> Analysis
computeAnalysis (User name surname _) = Analysis $
  HM.fromListWith (+) $ [(c,1) | c <- T.unpack name]
                     ++ [(c,1) | c <- T.unpack surname]

main :: IO ()
main = runPipelineTask_ "example1" (FullConfig "porcupine.yaml" "examples/data") mainTask
