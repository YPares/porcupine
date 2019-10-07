{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Arrows              #-}

import           Data.DocRecord
import qualified Data.Text.Lazy as T
import           Porcupine
import           Prelude hiding (id, (.))


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
myTask = proc () -> do
  (FV chars :& FV nums :& _) <-
    getOptions ["options"]
      (  docField @"chars"        "a"       "The chars to repeat"
      :& docField @"replications" [10::Int] "The numbers of replications"
      :& RNil) -< ()
  let txt = T.concat $
        zipWith (\s n -> T.replicate (fromIntegral n) (T.singleton s)) chars nums
  writeData resultFile -< txt

main :: IO ()
main = runLocalPipelineTask (FullConfig "example0.1" "example0_1.yaml" "." ()) myTask ()
