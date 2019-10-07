{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import           Data.DocRecord
import qualified Data.Text         as T
import           Porcupine.Run
import           Porcupine.Serials
import           Porcupine.Tasks


generateTxt :: Int -> T.Text
generateTxt n = T.replicate n "a"


resultFile :: DataSink T.Text
resultFile = dataSink ["result"] $
  somePureSerial (PlainTextSerial (Just "txt"))

myTask :: (LogThrow m) => PTask m () ()
myTask =
      getOption ["options"]
                (docField @"text-length" (10::Int)
                          "The length of the text to output")
  >>> arr generateTxt
  >>> writeData resultFile

main :: IO ()
-- main = simpleRunPTask myTask ()
main = runLocalPipelineTask (FullConfig "example0" "config.yaml" "." ()) myTask ()
