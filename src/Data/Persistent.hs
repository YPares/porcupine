{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Persistent
  ( RetrievingError(..)
  , persist
  , persistAtLoc
  , load
  , loadFromLoc
  , memo
  ) where

import           BasicPrelude

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Proxy             (Proxy (Proxy))
import qualified Data.Text              as T

import qualified Data.HumanSerializable as HS
import           Data.Locations         as Loc

type LoadPipelineM r m =
  ( LocationMonad m
  , MonadReader r m
  , HasRunContext r )

data RetrievingError
  = FileReadError Loc.Error
  | DecodingError Loc T.Text

instance Exception RetrievingError

instance Show RetrievingError where
  show (FileReadError loc) = "Impossible to read file " <> show loc
  show (DecodingError loc msg) =
    "Error while decoding file " <> show loc <> ": " <> T.unpack msg

-- | Save a value on a file.
-- The location of the file is defined by the @Ctx.ToLoc@ instance and the
-- serialization format by the @HS.Serializable@ one
persist
  :: forall a r m.
     ( LoadPipelineM r m
     , Localizable a r
     , HS.ToHuman a)
  => a
  -> m ()
persist x = do
  destFile <- localize (Proxy :: Proxy a)
  persistAtLoc x destFile

persistAtLoc
  :: forall a r m.
     ( LoadPipelineM r m
     , HS.ToHuman a)
  => a
  -> Loc
  -> m ()
persistAtLoc x loc = do
  let serializedX = HS.encode x
  writeLazyByte loc serializedX

loadFromLoc
  :: forall a r m.
     ( LoadPipelineM r m
     , HS.OfHuman a
     , MonadThrow m )
  => Loc
  -> m a
loadFromLoc origFile = do
  readLazyByte origFile
  >>= withReadError
  >>= decodeWithLoc origFile
  where
    withReadError :: Either Loc.Error b -> m b
    withReadError (Right x)  = return x
    withReadError (Left err) = throwM $ FileReadError err

    decodeWithLoc loc x = case HS.decode x of
      Right y  -> return y
      Left err -> throwM $ DecodingError loc err


-- | @load@ reconstructs a value of type @a@ from its
-- serialized version, or returns an error if it can't
load
  :: forall a r m.
     ( LoadPipelineM r m
     , Localizable a r
     , HS.OfHuman a)
  => m (Either RetrievingError a)
load = do
  origFile <- localize (Proxy :: Proxy a)
  try $ loadFromLoc origFile

-- | @memo a@ tries to retrieve an already evaluated version of @a@ from the
-- cache, otherwise returns @a@
memo
  :: forall a r m.
     ( LoadPipelineM r m
     , Localizable a r
     , HS.Serializable a)
  => a
  -> m a
memo x = do
  loadedX <- load
  case loadedX of
    Left _ -> do
      persist x
      pure x
    Right y -> pure y
