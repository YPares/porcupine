{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Porcupine.Serials
import Porcupine.Tasks
import Porcupine.Run
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

mainTask :: (LocationMonad m, KatipContext m) => PTask m () ()
mainTask =
  loadLast userFile >>> arr computeAnalysis >>> writeData analysisFile

computeAnalysis :: User -> Analysis
computeAnalysis (User name surname _) = Analysis $
  HM.fromListWith (+) $ [(c,1) | c <- T.unpack name]
                     ++ [(c,1) | c <- T.unpack surname]

main :: IO ()
main = runPipelineTask_ "example1" (FullConfig "porcupine.yaml" ".") mainTask
