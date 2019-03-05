{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Streaming.TaskPipelineUtils
  ( module S
  , Of(..)
  , MonadTrans(..)
  , MonadIO(..)
  , StreamFilter(..)
  , Copy(..)
  , (&)
  , S.mapM_
  , asConduit
  , intoSink
  , streamFolder
  , streamFolderRel
  , mapCopy
  , hoistCopy
  , With(..), elt, ann
  , StreamWith
  , mapStreamW
  , mapStreamWM )
  where

import           Control.Lens           hiding ((:>))
import           Control.Monad          (forM_)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Conduit           (ConduitT, Void, runConduit, (.|))
import           Data.Function          ((&))
import           GHC.Generics
import           Streaming
import           Streaming.Conduit      (asConduit, fromStreamSource)
import qualified Streaming.Prelude      as S
import           System.Directory       (doesDirectoryExist,
                                         getDirectoryContents)
import           System.FilePath        ((</>))


intoSink :: Monad m => ConduitT a Void m b -> Stream (Of a) m r -> m b
intoSink snk src = runConduit $ fromStreamSource src .| snk

-- TODO: rename to streamFolderRecursive
streamFolder :: (MonadIO m) => FilePath -> Stream (Of FilePath) m ()
streamFolder topPath = S.map (topPath </>) $ streamFolderRel topPath

-- TODO: rename to streamFolderRecursiveRel
streamFolderRel :: (MonadIO m) => FilePath -> Stream (Of FilePath) m ()
streamFolderRel topPath =
  aux ""
  where
    aux relPath = do
      names <- liftIO $ getDirectoryContents (topPath </> relPath)
      let properNames = filter (`notElem` [".", ".."]) names
      forM_ properNames $ \name -> do
        let path = relPath </> name
        isDirectory <- liftIO $ doesDirectoryExist path
        if isDirectory
          then aux path
          else S.yield path

-- | Generalizes 'partitionEithers' from Streaming.Prelude to fork a stream into
-- several substreams
-- TODO: This language of filters needs an introduction.
-- What problem do they solve? Why bare streams aren't suitable for the task?
class StreamFilter s where
  type Wanted s :: *
  type Split s (m :: * -> *) :: * -> *
  filters :: (Monad m) => s -> Stream (Of (Wanted s)) m r -> Split s m r

instance StreamFilter (a -> Bool) where
  type Wanted (a -> Bool) = a
  type Split (a -> Bool) m = Stream (Of a) m
  filters = S.filter

instance StreamFilter (a -> Maybe b) where
  type Wanted (a -> Maybe b) = a
  type Split (a -> Maybe b) m = Stream (Of b) m
  filters = S.mapMaybe

instance StreamFilter (a -> Either b c) where
  type Wanted (a -> Either b c) = a
  type Split (a -> Either b c) m = Stream (Of b) (Stream (Of c) m)
  filters f = S.partitionEithers . S.map f

instance (StreamFilter s', Wanted s' ~ a) => StreamFilter (Of (a -> Bool) s') where
  type Wanted (Of (a -> Bool) s') = a
  type Split (Of (a -> Bool) s') m = Stream (Of a) (Split s' m)
  filters (f :> s') = hoist (filters s') . S.partition f

instance (StreamFilter s', Wanted s' ~ a) => StreamFilter (Of (a -> Maybe b) s') where
  type Wanted (Of (a -> Maybe b) s') = a
  type Split (Of (a -> Maybe b) s') m = Stream (Of b) (Split s' m)
  filters (f :> s') = filters (f' :> s')
    where f' a = case f a of
            Just b  -> Left b
            Nothing -> Right a

instance (StreamFilter s', Wanted s' ~ c) => StreamFilter (Of (a -> Either b c) s') where
  type Wanted (Of (a -> Either b c) s') = a
  type Split (Of (a -> Either b c) s') m = Stream (Of b) (Split s' m)
  filters (f :> s') = hoist (filters s') . S.partitionEithers . S.map f

data Copy = Copy

instance (StreamFilter s') => StreamFilter (Of Copy s') where
  type Wanted (Of Copy s') = Wanted s'
  type Split (Of Copy s') m = Stream (Of (Wanted s')) (Split s' m)
  filters (Copy :> s') = hoist (filters s') . S.copy

-- | Copies stream elements to a layer underneath after applying a function on
-- them
-- TODO: Motivate.
mapCopy
  :: Monad m
  => (a -> b) -> Stream (Of a) (Stream (Of b) m) r -> Stream (Of a) (Stream (Of b) m) r
mapCopy f stream = S.for stream $ \x -> do
  S.yield x
  lift $ S.yield $ f x

-- | A version of mapCopy that takes the whole substream of copied values and
-- merges it downwards
-- TODO: Motivate.
hoistCopy
  :: (Monad m)
  => (forall n s. (Monad n) => Stream (Of a) n s -> Stream (Of b) n s)
  -> Stream (Of a) (Stream (Of b) m) r -> Stream (Of a) (Stream (Of b) m) r
hoistCopy g stream =
  S.copy stream & hoist (S.effects . flip S.for (lift . S.yield) . g)

-- | Just a simple tuple to annotate stream elements. It is strict in the
-- annotation.
-- TODO: Motivate. How is this better than a primitive pair?
data t `With` ann = With { _ann :: !ann, _elt :: t }
  deriving (Eq, Generic)

makeLenses ''With

instance (ToJSON t, ToJSON ann) => ToJSON (t `With` ann)
instance (FromJSON t, FromJSON ann) => FromJSON (t `With` ann)

type StreamWith id a = Stream (Of (a `With` id))

-- TODO: Maybe this function can be defined more generally when the stream
-- elements are functors.
mapStreamW :: Monad m => (a -> b) -> StreamWith ann a m r -> StreamWith ann b m r
mapStreamW f = S.map $ \case
  With pid a -> With pid (f a)

mapStreamWM :: Monad m => (a -> m b) -> StreamWith ann a m r -> StreamWith ann b m r
mapStreamWM f = S.mapM $ \case
  With pid a -> With pid <$> f a
