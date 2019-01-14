{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}
{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Data.Locations.Accessors.HTTP where

import           Control.Exception.Safe
import           Control.Monad.ReaderSoup
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Streaming    as BSS
import           Data.Function                ((&))
import           Data.Locations.Accessors
import           Data.Locations.Loc
import           Network.HTTP.Simple
import           Streaming
import qualified Streaming.Conduit            as SC


-- | The context is just empty for now, but we might want to add for instance a
-- Manager in the future.
data HTTPContext = HTTPContext

type instance ContextFromName "http" = HTTPContext

instance SoupContext HTTPContext (ReaderT HTTPContext) where
  toReaderT = id
  fromReaderT = id

useHTTP :: ContextRunner (ReaderT HTTPContext) m
useHTTP = ContextRunner $ flip runReaderT HTTPContext

makeReq :: MonadThrow m => LocOf "http" -> m Request
makeReq (H loc@RemoteFile{rfProtocol=p})
  | p == "http" || p == "https" = parseRequest $ show loc
makeReq (H loc) = error $ show loc ++ " isn't an http(s) URL"

instance (MonadResource m, MonadMask m)
  => LocationAccessor m "http" where
  newtype LocOf "http" = H Loc
    deriving (ToJSON)
  locExists _ = return True
  writeBSS l bss = do
    req <- makeReq l
    (bs :> r) <- BSS.toLazy bss
    _ <- httpNoBody $
      req & setRequestMethod "POST"
          & setRequestBodyLBS bs
    return r
  readBSS l f = do
    req <- makeReq l
    f $ SC.toBStream $ httpSource req getResponseBody

instance FromJSON (LocOf "http") where
  parseJSON v = do
    loc <- parseJSON v
    case loc of
      RemoteFile{rfProtocol=p}
        | p == "http" || p == "https" ->
          return $ H loc
      _ -> fail "Doesn't use http(s) protocol"
