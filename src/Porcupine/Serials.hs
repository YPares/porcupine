module Porcupine.Serials
  ( module Data.Locations.SerializationMethod
  , VirtualFile(..)
  , BidirVirtualFile, DataSource, DataSink
  , LocationTreePathItem
  , unusedByDefault, documentedFile
  , bidirVirtualFile, dataSource, dataSink
  , ensureBidirFile, makeSource, makeSink )
where

import Data.Locations.SerializationMethod
import Data.Locations.VirtualFile
