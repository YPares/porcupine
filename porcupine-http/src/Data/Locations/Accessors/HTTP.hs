{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
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
import           Data.Aeson
import qualified Data.ByteString.Streaming    as BSS
import           Data.Function                ((&))
import           Data.Locations.Accessors
import           Data.Locations.Loc
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           GHC.Generics                 (Generic)
import           Network.HTTP.Simple
import qualified Network.Mime                 as Mime
import           Streaming
import qualified Streaming.Conduit            as SC


-- | The context is just a dummy one for now, but we might want to add for
-- instance a Manager in the future.
data HTTPContext = HTTPContext

type instance ContextFromName "http" = HTTPContext

instance SoupContext HTTPContext (ReaderT HTTPContext) where
  toReaderT = id
  fromReaderT = id

useHTTP :: ContextRunner (ReaderT HTTPContext) m
useHTTP = ContextRunner $ flip runReaderT HTTPContext

makeReq :: MonadThrow m => Loc -> m Request
makeReq loc@RemoteFile{rfProtocol=p}
  | p == "http" || p == "https" = parseRequest $ show loc
makeReq loc = error $ show loc ++ " isn't an http(s) URL"

instance (MonadResource m, MonadMask m)
  => LocationAccessor m "http" where
  data GLocOf "http" a = HTTPLoc
    { url :: URLLikeLoc a
    , writeMethod :: T.Text
    , readMethod :: T.Text
    , requiredType :: Maybe T.Text
    } deriving (Functor, Foldable, Traversable, Generic, ToJSON)
  locExists _ = return True
  writeBSS l bss = do
    req <- makeReq $ url l
    (bs :> r) <- BSS.toLazy bss
    _ <- httpNoBody $
      req & setRequestMethod (TE.encodeUtf8 $ writeMethod l)
          & setRequestBodyLBS bs
    return r
  readBSS l f = do
    req <- makeReq $ url l
    let
      -- XXX We silently ignore if the extension has no default mime type
      -- associated. Should we warn here?
      mimeType = flip Map.lookup Mime.defaultMimeMap =<< requiredType l
      setMimeType = case mimeType of
        Nothing    -> id
        Just mtype -> setRequestHeader "Accept" [ mtype ]
    f $ SC.toBStream $
      httpSource (setMimeType $ setRequestMethod (TE.encodeUtf8 $ readMethod l) req)
                 getResponseBody

instance (MonadResource m, MonadMask m) => MayProvideLocationAccessors m "http"

instance (IsLocString a) => Show (GLocOf "http" a) where
  show = show . url

getURLType :: URLLikeLoc a -> Maybe T.Text
getURLType url = case getLocType url of
  ""  -> Nothing
  ext -> Just $ T.pack ext  -- TODO: check that the extension is a valid one
                            -- (from the list in mime-types)

instance (IsLocString a) => FromJSON (GLocOf "http" a) where
  parseJSON (Object v) = do
    url <- v .: "url"
    HTTPLoc url <$> (v .: "writeMethod" <|> pure "POST")
                <*> (v .: "readMethod" <|> pure "GET")
                <*> ((Just <$> v .: "requiredType") <|> pure (getURLType url))
  parseJSON v@(String _) = do
    url <- parseJSON v
    case url of
      RemoteFile{rfProtocol=p}
        | p == "http" || p == "https" ->
          return $ HTTPLoc url "POST" "GET" $ getURLType url
      _ -> fail "Doesn't use http(s) protocol"
  parseJSON _ = fail
    "Must be an http(s) URL or a JSON object with fields url,writeMethod,readMethod"

instance TypedLocation (GLocOf "http") where
  getLocType l = T.unpack . fromMaybe "" $ requiredType l
  setLocType l f = l{requiredType = Just . T.pack . f $ getLocType l}
  addSubdirToLoc l d = l{url = addSubdirToLoc (url l) d}
  useLocAsPrefix l p = l{url = useLocAsPrefix (url l) p}
