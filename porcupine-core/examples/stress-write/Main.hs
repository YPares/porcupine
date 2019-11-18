{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Arrows            #-}

module Main where

import Porcupine
import qualified Data.Text.Lazy as T
import Control.Concurrent

import           Prelude                            hiding (id, (.))


output :: BidirVirtualFile T.Text
output = plainTextVFile ["output"]

main :: IO ()
main = runLocalPipelineTask (FullConfig "stress-write" "stress-write.yaml" "." ()) myTask ()

myTask :: (LogThrow m) => PTask m () ()
myTask = proc () -> do  -- proc can't pattern match directly on GADTs
  opts <- getOptions ["options"] defOpts -< ()
  let (FV numF :& FV numC :& FV waitMS :& _) = opts
      mkTxt = T.replicate (fromIntegral numC) . T.pack . show
  parMapTask "fileNum" (proc (_,txt,waitMS) -> do
                           writeData output -< txt
                           ioTask -< threadDelay (waitMS*1000)
                           loadData output -< ())
    -< [ (x,mkTxt x,waitMS) | x <- [(1::Int)..numF] ]
  returnA -< ()
  where
    defOpts = docField @"num-files" (1000::Int) "The number of files to write"
              :& docField @"num-chars" (1000::Int) "The number of chars per file"
              :& docField @"ms-delay" (0::Int) "The waiting time between writes & reads"
              :& RNil
