{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Arrows            #-}

module Main where

import Porcupine
import qualified Data.Text.Lazy as T

import           Prelude                            hiding (id, (.))


output :: BidirVirtualFile T.Text
output = plainTextVFile ["output"]

main :: IO ()
main = runLocalPipelineTask (FullConfig "stress-write" "stress-write.yaml" "." ()) myTask ()

myTask :: (LogThrow m) => PTask m () ()
myTask = proc () -> do  -- proc can't pattern match directly on GADTs
  opts <- getOptions ["options"] defOpts -< ()
  let (FV numF :& FV numC :& _) = opts
      mkTxt = T.replicate (fromIntegral numC) . T.pack . show
  seqMapTask "fileNum" (arr snd >>> writeData output >>> loadData output)
    -< [ (x,mkTxt x) | x <- [(1::Int)..numF] ]
  returnA -< ()
  where
    defOpts = docField @"num-files" (1000::Int) "The number of files to write"
              :& docField @"num-chars" (1000::Int) "The number of chars per file"
              :& RNil
