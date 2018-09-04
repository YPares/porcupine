{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Data.DocRecord.OptParse
  (RecFromCLI(..), FieldFromCLI
  ,RecordUsableWithCLI
  ,SourceTag(..)
  ,SourcedDocField
  ,rmTags,tagWithDefaultSource,tagWithYamlSource

  ,parseRecFromCLI
  )
where

import           Control.Lens
import           Data.Bifunctor       (first)
import           Data.DocRecord
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import qualified Data.Vinyl.Functor   as F
import           Data.Yaml            (FromJSON, ToJSON)
import qualified Data.Yaml            as Y
import           GHC.TypeLits         (Symbol)
import           Options.Applicative


data Marker = Flag | Regular | None

type family MarkerOf a where
  MarkerOf MD   = None
  MarkerOf Bool = Flag
  MarkerOf a    = Regular

-- | Identifies the source of a value in the configuration, so that we can be
-- sure to override the right values with the right values.
--
-- The Ord instance makes it so CLI overrides YAML and YAML overrides Default.
data SourceTag = Default | YAML | CLI
  deriving (Eq, Ord, Show)

instance Monoid SourceTag where
  mempty = Default

instance Semigroup SourceTag where
  a <> b = if a > b then a else b

instance NamedFieldTag SourceTag where
  defaultTag = Default
  tagFromDoc _ = Default

-- | A DocField with a Source tag attached
type SourcedDocField = Tagged SourceTag `F.Compose` DocField

rmTags :: Rec SourcedDocField r -> Rec DocField r
rmTags r = (\(F.Compose (Tagged _ x)) -> x) <<$>> r

tagWithDefaultSource :: Rec DocField rs -> Rec SourcedDocField rs
tagWithDefaultSource r = F.Compose . Tagged Default <<$>> r

tagWithYamlSource :: Rec DocField rs -> Rec SourcedDocField rs
tagWithYamlSource r = F.Compose . Tagged YAML <<$>> r


-- | Is satisfied when every field of a 'DocRec' @rs@ is transformable from & to
-- JSON and gettable from the CLI.
type RecordUsableWithCLI rs =
  ( RecFromCLI (Rec (Tagged SourceTag `F.Compose` DocField) rs)
  , ToJSONFields rs, FromJSON (Rec PossiblyEmptyField rs) )

class (FromJSON (Snd r), m ~ MarkerOf (Snd r)) => FieldFromCLI_ m r where
  fieldFromCLI
    :: (NamedField f, FieldWithTag T.Text f, FieldWithTag SourceTag f)
    => String
    -> f r
    -> Parser (f r)

instance (FromJSON a, ToJSON a, MarkerOf a ~ Regular, ShowPath s) => FieldFromCLI_ Regular (s:|:a) where
  fieldFromCLI flagName field =
    option reader
     (  long flagName
     <> help (T.unpack $ field^.fieldTag)
     <> case field^.rfield of
          Nothing -> mempty
          Just x  -> value field <> showDefaultWith (const $ showJson x))
    where
      reader = eitherReader $ \string -> do
        newVal <- first show <$> Y.decodeEither' . encodeUtf8 . T.pack $ string
        return $ field & rfield .~ Just newVal
                       & fieldTag .~ CLI
                         -- We set the source of the new value so this field has
                         -- a higher priority than the equivalent field coming
                         -- from the Yaml file
      showJson = T.unpack . decodeUtf8 . Y.encode

instance (ShowPath s) => FieldFromCLI_ Flag (s:|:Bool) where
  fieldFromCLI flagName field =
    flag defState flipState
        (  long (flagPrefix++flagName)
        <> help (docPrefix ++ (T.unpack $ field^.fieldTag)))
    where (isOn, (defState, flipState)) = case field^.rfield of
            Nothing  -> (False, states False)
            Just val -> (val, states val)
          states v = ( field & rfield .~ Just v  -- Default value. We keep the same value source.
                     , field & rfield .~ Just (not v)
                             & fieldTag .~ CLI -- Flipped value. We set the new
                                               -- value source.
                     )
          (flagPrefix, docPrefix) = if isOn then ("no-", "Deactivate: ") else ("", "")

instance (FromJSON a, MarkerOf a ~ None) => FieldFromCLI_ None (s:|:a) where
  fieldFromCLI _ _ = empty

type FieldFromCLI a = FieldFromCLI_ (MarkerOf (Snd a)) a

class RecFromCLI a where
  parseRecFromCLI_ :: HM.HashMap [T.Text] String -> a -> Parser a
  allPaths :: a -> [[T.Text]]

instance RecFromCLI (Rec (f :: PathWithType [Symbol] * -> *) '[]) where
  parseRecFromCLI_ _ _ = pure RNil
  allPaths _           = []

instance (NamedField f, FieldWithTag T.Text f, FieldWithTag SourceTag f
         , FieldFromCLI (s:|:t), RecFromCLI (Rec f rs), ShowPath s)
  => RecFromCLI (Rec f ((s:|:t) ': rs)) where
  parseRecFromCLI_ fieldNames (f1 :& rest) = (:&)
    <$> (   fieldFromCLI (fieldNames HM.! fieldPathList f1) f1
        <|> pure f1 )
    <*> parseRecFromCLI_ fieldNames rest
  allPaths (f1 :& rest) = fieldPathList f1 : allPaths rest

parseRecFromCLI
  :: forall (rs :: [PathWithType [Symbol] *]) f. (RecFromCLI (Rec f rs))
  => Rec f rs -> Parser (Rec f rs)
parseRecFromCLI defaultRec = parseRecFromCLI_ disambMap defaultRec
  where
    disambMap = HM.fromList $ concatMap (disambOn 1) $ ambiguousOn 1 $ allPaths defaultRec
    nameOn n = reverse . take n . reverse
    ambiguousOn n paths =
      HM.elems $ HM.fromListWith (++) $
        map (\p -> (nameOn n p, [p])) paths
    disambOn n [uniq] = [(uniq, T.unpack $ T.intercalate (T.pack "-") $ nameOn n uniq)]
    disambOn n ps     = concatMap (disambOn (n+1)) $ ambiguousOn (n+1) ps
