{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

import           Data.DocRecord
import qualified Data.Text as T
import           Porcupine.Run
import           Porcupine.Serials
import           Porcupine.Tasks
import           Prelude hiding (id, (.))


generateTxt :: Int -> T.Text
generateTxt n = T.replicate n "a" <> T.replicate 5 "b"


yzCompress :: T.Text -> T.Text
yzCompress = T.concat . map counts . T.group
  where
    counts s = T.pack (show (T.length s)) <> T.take 1 s <> ","

resultFile :: DataSink T.Text
resultFile = dataSink ["result"] $
     somePureSerial (PlainTextSerial (Just "txt"))
  <> lmap yzCompress
          (somePureSerial (PlainTextSerial (Just "yz")))

myTask :: (LogThrow m) => PTask m () ()
myTask =
      getOption ["options"]
                (docField @"text-length" (10::Int)
                          "The length of the text to output")
  >>> arr generateTxt
  >>> writeData resultFile

main :: IO ()
main = runLocalPipelineTask (FullConfig "example0.1" "config.yaml" "." ()) myTask ()
