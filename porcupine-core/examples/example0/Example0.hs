{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}

import           Data.DocRecord
import qualified Data.Text as T
import           Porcupine


resultFile :: DataSink T.Text
resultFile = dataSink ["result"] $
  somePureSerial (PlainTextSerial (Just "txt"))

myTask :: (LogThrow m) => PTask m () ()
myTask =
      getOptions ["options"]
        (  docField @"chars"        "A"      "The characters to repeat"
        :& docField @"text-length" (10::Int) "The length of the text to output"
        :& RNil)
  >>> arr (\(OptF char :& OptF len :& _)
            -> T.replicate len char)
  >>> writeData resultFile

main :: IO ()
-- main = simpleRunPTask myTask ()
main = runLocalPipelineTask (FullConfig "example0" "config.yaml" "." ()) myTask ()
