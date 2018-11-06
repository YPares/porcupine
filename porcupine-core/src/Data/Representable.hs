{-# LANGUAGE OverloadedStrings #-}

module Data.Representable where

import           Control.Applicative
import           Data.Text


-- | A class for small objects that can be turned into a Text which will then be
-- stored in JSON String objects
class Representable a where
  toTextRepr :: a -> Text
  fromTextRepr :: (Alternative f) => Text -> f a

instance (Representable a) => Representable (Maybe a) where
  toTextRepr Nothing  = ""
  toTextRepr (Just x) = toTextRepr x
  fromTextRepr "" = pure Nothing
  fromTextRepr t  = Just <$> fromTextRepr t

instance Representable Text where
  toTextRepr = id
  fromTextRepr = pure
