{-# LANGUAGE Arrows            #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import           Data.DocRecord
import qualified Data.Text.Lazy as T
import           Porcupine


resultFile :: BidirVirtualFile T.Text
resultFile = plainTextVFile ["result"]

myTask :: (LogThrow m) => PTask m () ()
myTask = proc () -> do
  (FV char :& FV num :& _) <- getMyOptions -< ()
  let txt = T.replicate (fromIntegral num) (T.singleton char)
  writeData resultFile -< txt
  where
    getMyOptions =
      getOptions ["options"]
      (  docField @"char"         'a'       "The character to repeat"
      :& docField @"replications" (10::Int) "The number of replications"
      :& RNil)

main :: IO ()
-- main = simpleRunPTask myTask ()
main = runLocalPipelineTask (FullConfig "example0" "example0.yaml" "." ()) myTask ()
