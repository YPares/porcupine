{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- FIXME: shouldn't need this

-----------------------------------------------------------------------------
-- |
-- Module :  "Data.DocRecord"
--
-- This modules provides extensible records where each field includes a
-- documentation.
--
-----------------------------------------------------------------------------

module Data.DocRecord
  (
  -- * Examples
  -- $setup
  -- * Re-Exports
    module Data.Vinyl.Core
  , module Data.Vinyl.Lens
  , module Data.Vinyl.Derived
  , module Data.Vinyl.Curry
  , type (++)
  , type AllFst, type AllSnd
  -- * Types
  , PathWithType(..)
  , FieldWithTag, fieldTag
  , Field(..)
  , Tagged(..)
  , WithDoc
  , PossiblyEmpty(..)
  , PossiblyEmptyField
  , pattern PEField
  , type DocField
  , pattern DocField
  , DocRec
  , FieldTypes
  , IdentityField
  , NamedField(..)
  , NamedFieldTag(..)
  , MissingValueReason(..)
  , IntermediaryLevel
  , FlattenedLevel
  , HasField, Includes, EquivalentTo
  , type Difference, type Intersection
  , ToJSONFields
  , RecBijection(..)
  , ShowPath(..)
  , ApplyRec(..)
  , MD(..)
  , type Fst
  , type Snd
  -- * Utils
  , removeDoc
  , withoutDef
  , getPossiblyEmpty
  , chooseHighestPriority
  , fld
  , runcurryF
  , runcurryAF
  , docField
  , itmLevel
  , fieldPath
  , fieldPathList
  , fieldFromDef
  , fieldNoDef
  , singleton
  , useDef
  , fromJSONAs
  , (^^.), (^^?), (^^?!), (%%~), (..~)
  , renamedAs
  , rsubset, rcast, rreplace
  , rcastAs, rsplit, rsplitFrom, rdifference, rintersection
  , PrefixPath(..), rinclude, (-.)
  , rdrill
  , rsplitDrill
  , rfoldSubset
  , funder
  , runder
  , (-/)
  , withSameFields
  , (&:)
  , recFrom
  , invertRecBij, (<<|>>), bijectField, bijectField', renameField, addConstField
  , bijectUnder
  , showDocumentation
  ) where

import           Control.Applicative
import qualified Control.Category     as Cat
import qualified Control.Lens         as L
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe           (fromMaybe)
import           Data.Proxy
import qualified Data.Text            as T
import           Data.Typeable
import           Data.Vinyl.Core
import           Data.Vinyl.Curry
import           Data.Vinyl.Derived   hiding (HasField, rfield, (=:))
import qualified Data.Vinyl.Functor   as F
import           Data.Vinyl.Lens      (RElem, RSubset, rlens)
import qualified Data.Vinyl.Lens      as VL
import           Data.Vinyl.TypeLevel hiding (Fst, Snd)
import           GHC.Exts             (Constraint)
import           GHC.TypeLits         (ErrorMessage (..), KnownSymbol, Symbol,
                                       TypeError, symbolVal)

-- $setup
-- Here is an example of use:
--
-- >>> :set -XDataKinds -XTypeApplications -XOverloadedStrings
-- >>> import Data.Function ((&))
-- >>> import qualified Data.Text.IO as T
-- >>> import Data.Aeson (toJSON)
-- >>> let age  = docField   @"age"  @Int    12  "This is the field giving the age"
-- >>> let size = docField   @"size" @Double 130 "This is the field giving the size (in cm)"
-- >>> let name = fieldNoDef @"name" @String     "This is the field giving the name"
-- >>> let defaultPerson = age :& name :& size :& RNil
-- >>> let namedDefault = name ..~ "Bernard" $ defaultPerson
-- >>> defaultPerson
-- {age =: 12
-- , name (empty: NoDefault)
-- , size =: 130.0
-- }
--
-- A DocRec can be serialized/deserialized to/from Json or Yaml.
--
-- >>> let j = toJSON namedDefault
-- >>> j
-- Object (fromList [("size",Number 130.0),("age",Number 12.0),("name",String "Bernard")])
--
-- >>> fromJSONAs defaultPerson j
-- Success {age =: 12
-- , name =: "Bernard"
-- , size =: 130.0
-- }

-- | Explains why a fields contains no value
data MissingValueReason = NoDefault | MissingValueInJSON
  deriving (Show, Eq, Ord)

-- | Wraps a field so that it can have no value
newtype PossiblyEmpty a = PE (Either MissingValueReason a)
  deriving (Functor, Applicative)

instance (Eq a) => Eq (PossiblyEmpty a) where
  PE (Right a) == PE (Right b) = a == b
  PE (Left _) == PE (Left _) = True
  _ == _ = False
instance Ord a => Ord (PossiblyEmpty a) where
  PE (Right a) `compare` PE (Right b) = a `compare` b
  PE (Left _) `compare` PE (Left _) = EQ
  _ `compare` _ = LT
instance (Show (f (s:|:a)), ShowPath s)
      => Show (PossiblyEmpty (f (s:|:a))) where
  show x = case x of
    PE (Right a) -> show a
    PE (Left r)  ->
      T.unpack (showPath (Proxy @s)) ++ " (empty: " ++ show r ++ ")\n"
instance (Semigroup a) => Semigroup (PossiblyEmpty a) where
  PE (Right x) <> PE (Right y) = PE $ Right $ x<>y
  PE (Left _) <> PE (Right x) = PE $ Right x
  PE (Right x) <> PE (Left _) = PE $ Right x
  PE (Left x) <> PE (Left y) = PE $ Left $ max x y
instance (Semigroup a) => Monoid (PossiblyEmpty a) where
  mempty = PE (Left NoDefault)


-- | Wraps a field and gives it some tag
data Tagged tag a = Tagged { tagFromTagged   :: tag
                           , valueFromTagged :: a }

-- | Wraps a field and gives it some documentation
type WithDoc = Tagged T.Text

instance (Eq a) => Eq (Tagged tag a) where
  Tagged _ a == Tagged _ b = a == b
instance (Ord a) => Ord (Tagged tag a) where
  Tagged _ a `compare` Tagged _ b = a `compare` b
instance (Show a) => Show (Tagged tag a) where
  show (Tagged _ a) = show a
-- | The tag is right-biased
instance (Semigroup a) => Semigroup (Tagged tag a) where
  Tagged _ x <> Tagged tag y = Tagged tag (x<>y)
instance (Monoid tag, Monoid a) => Monoid (Tagged tag a) where
  mempty = Tagged mempty mempty
instance Functor (Tagged tag) where
  fmap f (Tagged d a) = Tagged d (f a)
instance (Monoid tag) => Applicative (Tagged tag) where
  pure = Tagged mempty
  Tagged t1 f <*> Tagged t2 x = Tagged (t1<>t2) (f x)

-- | When two fields are tagged with an Ord, return the field with the highest
-- one. Right field is returned if both tags are equal.
chooseHighestPriority
  :: Ord a
  => F.Compose (Tagged a) f x
  -> F.Compose (Tagged a) f x
  -> F.Compose (Tagged a) f x
chooseHighestPriority f1@(F.Compose (Tagged s1 _))
                      f2@(F.Compose (Tagged s2 _)) =
  if s2 >= s1 then f2 else f1

-- | Just a type-level tuple, for easier to read type signatures
data PathWithType a b = a :|: b

-- | The most basic field. We don't use ElField from vinyl so we can use the
-- PathWithType kind instead of tuple and paths instead of just names.
data Field (pathAndType :: ( PathWithType [Symbol] * )) where
  Field :: (ShowPath s) => !t -> Field (s :|: t)

deriving instance (Eq a) => Eq (Field (s:|:a))
deriving instance (Ord a) => Ord (Field (s:|:a))
instance (Show t, ShowPath s) => Show (Field (s:|:t)) where
  show (Field x) =
    T.unpack (showPath (Proxy @s)) ++ " =: " ++ show x ++ "\n"

instance (Semigroup t) => Semigroup (Field (s :|: t)) where
  Field a <> Field b = Field $ a<>b
instance (ShowPath s, Monoid t) => Monoid (Field (s :|: t)) where
  mempty = Field mempty

-- instance (Show (f (g a))) => Show (F.Compose f g a) where
--   show (F.Compose x) = show x

type PossiblyEmptyField = F.Compose PossiblyEmpty Field

peToMb :: (NamedField field, ShowPath s) => Either r (field (s:|:a)) -> Maybe a
peToMb (Left _)  = Nothing
peToMb (Right f) = f L.^. rfield

peFromMb
  :: (ShowPath s, NamedField field)
  => Maybe a -> Either MissingValueReason (field (s ':|: a))
peFromMb Nothing  = Left NoDefault
peFromMb (Just x) = Right $ fromValue x

pattern PEField
  :: ( NamedField f, ShowPath s)
  => Maybe a -> F.Compose PossiblyEmpty f (s:|:a)
pattern PEField v <- F.Compose (PE (peToMb -> v)) where
  PEField v = F.Compose (PE (peFromMb v))

type DocField = F.Compose WithDoc PossiblyEmptyField

removeDoc :: F.Compose WithDoc f st -> f st
removeDoc (F.Compose (Tagged _ x)) = x

withoutDef :: F.Compose WithDoc (F.Compose PossiblyEmpty f) st
           -> F.Compose WithDoc (F.Compose PossiblyEmpty f) st
withoutDef (DocField doc _) = DocField doc (Left NoDefault)

getPossiblyEmpty
  :: F.Compose PossiblyEmpty f st -> Either MissingValueReason (f st)
getPossiblyEmpty (F.Compose (PE x)) = x

pattern DocField :: T.Text
                 -> Either MissingValueReason (g x)
                 -> F.Compose WithDoc (F.Compose PossiblyEmpty g) x
pattern DocField doc mbf = F.Compose (Tagged doc (F.Compose (PE mbf)))

-- | A extensible record of documented fields with values
type DocRec = Rec DocField

-- | To forget the field paths and get only the field types
type family FieldTypes rs where
  FieldTypes '[] = '[]
  FieldTypes ((s:|:t) : rs) = t : FieldTypes rs

fieldPathList :: forall st p. (ShowPath (Fst st)) => p st -> [T.Text]
fieldPathList _ = showPathList (Proxy @(Fst st))

fieldPath :: forall st p. (ShowPath (Fst st)) => p st -> T.Text
fieldPath _ = showPath (Proxy @(Fst st))

class ShowPath p where
  showPathList :: proxy p -> [T.Text]
  showPath :: proxy p -> T.Text
  showPath p = T.intercalate (T.pack ".") (showPathList p)
instance ShowPath '[] where
  showPathList _ = []
instance (ShowPath ps, KnownSymbol p) => ShowPath (p ': ps) where
  showPathList _ = T.pack (symbolVal (Proxy @p)) : showPathList (Proxy @ps)

-- | Creates missing levels of the json tree upon traversing
jsonAtPath :: [T.Text] -> L.Lens' (Maybe Value) (Maybe Value)
jsonAtPath [] f x = f x
jsonAtPath (p:ps) f val = rebuild <$> recur
  where
    (obj, recur) = case val of
      Just (Object o) -> (o,        jsonAtPath ps f $ HM.lookup p o)
      _               -> (HM.empty, jsonAtPath ps f $ Just $ Object HM.empty)
    rebuild Nothing  = Just $ Object $ HM.delete p obj
    rebuild (Just v) = Just $ Object $ HM.insert p v obj

instance FromJSON (Rec PossiblyEmptyField '[]) where
  parseJSON (Object _) = pure RNil
  parseJSON _          = mempty
instance (FromJSON t, FromJSON (Rec PossiblyEmptyField rs), ShowPath s)
  => FromJSON (Rec PossiblyEmptyField ((s:|:t) ': rs)) where
  parseJSON v = rebuild <$> parseField (L.view (jsonAtPath p) (Just v))
                        <*> parseJSON @(Rec PossiblyEmptyField rs) v
    where p = showPathList (Proxy @s)
          rebuild mbV rest = F.Compose (PE (Field <$> mbV)) :& rest
          parseField mbX = if x == Null  -- We allow unexisting values
                           then parsing <|> pure (Left MissingValueInJSON)
                           else parsing  -- But not values with a bad type
            where parsing = Right <$> parseJSON x
                  x = fromMaybe Null mbX

-- | Just sets the docstrings to empty
instance (RMap rs, FromJSON (Rec PossiblyEmptyField rs)) => FromJSON (Rec DocField rs) where
  parseJSON v = rmap toDoc <$> parseJSON v
    where toDoc = F.Compose . Tagged ""

-- | Just a shortcut to fix the record type that we are expecting in return
--
-- >>> let (Success p) = fromJSONAs defaultPerson j
-- >>> p
-- {age =: 12
-- , name =: "Bernard"
-- , size =: 130.0
-- }
--
-- @
--          ^^^ At this step (when pattern matching on Success)
--           we can re-order the fields of defaultPerson
--           or even get just a subset of them
-- @
--
fromJSONAs :: (FromJSON x) => x -> Value -> Result x
fromJSONAs _ = fromJSON

type family Fst a where
  Fst (a:|:b) = a
type family Snd a where
  Snd (a:|:b) = b
type family AllFst c p :: Constraint where
  AllFst c (r ': rs) = (c (Fst r), AllFst c rs)
  AllFst c '[] = ()
type family AllSnd c p :: Constraint where
  AllSnd c (r ': rs) = (c (Snd r), AllSnd c rs)
  AllSnd c '[] = ()

instance (ToJSON `AllSnd` rs, ShowPath `AllFst` rs)
      => ToJSON (Rec PossiblyEmptyField rs) where
  toJSON RNil = Object mempty
  toJSON x = into (Object mempty) x --object . toPairs
    where
      into :: forall rs'. (ToJSON `AllSnd` rs', ShowPath `AllFst` rs')
           => Value -> Rec PossiblyEmptyField rs' -> Value
      into o RNil = o
      into o (field@(F.Compose (PE innerField)) :& fs) = into
        (fromMaybe (error "Should not happen!") $
            L.set (jsonAtPath $ fieldPathList field)
                  (Just $ case innerField of
                     Right (Field v) -> toJSON v
                     Left _          -> Null)
                  (Just o))
        fs

-- | Just ignores the docstrings
instance (RMap rs, ToJSON `AllSnd` rs, ShowPath `AllFst` rs)
      => ToJSON (Rec DocField rs) where
  toJSON = toJSON . rmap removeDoc

-- | A shortcut to ensure all fields in list are convertible to JSON
type ToJSONFields fields =
  (ToJSON `AllSnd` fields, Typeable `AllSnd` fields, ShowPath `AllFst` fields)

-- | Displays all the field names, types, and documentation contained in a record
--
-- >>> T.putStrLn $ showDocumentation defaultPerson
-- age (Int) : This is the field giving the age
-- name ([Char]) : This is the field giving the name
-- size (Double) : This is the field giving the size (in cm)
showDocumentation
  :: forall rs field. (ShowPath `AllFst` rs, Typeable `AllSnd` rs)
  => Int  -- ^ Character limit for types
  -> Rec (F.Compose WithDoc field) rs
  -> T.Text
showDocumentation charLimit (f :& fs) =
  showF f <> case fs of
               (_ :& _) -> "\n" <> showDocumentation charLimit fs
               RNil     -> ""
  where
    showF :: forall r. (ShowPath (Fst r), Typeable (Snd r))
          => F.Compose WithDoc field r -> T.Text
    showF (F.Compose (Tagged  doc _)) =
      showPath (Proxy @(Fst r))
      <> " :: " <> T.pack (cap $ show $ typeRep $ Proxy @(Snd r))
      <> " : " <> doc
    cap x | length x >= charLimit = take charLimit x ++ "..."
          | otherwise = x
showDocumentation _ RNil = ""

-- | Redefines @rfield@ and @(=:)@ from Data.Vinyl.Derived so they can work over
-- different kinds of fields.
class NamedField field where
  -- | Lens to the payload of a field
  rfield :: (ShowPath s) => L.Lens (field (s :|: a)) (field (s :|: b)) (Maybe a) (Maybe b)

  -- | Construct a NamedField from a value
  fromValue :: (ShowPath s) => a -> field (s :|: a)

  -- | Transform the value inside the field if there is one
  mapField :: (ShowPath s)
           => (t -> t') -> field (s:|:t) -> field (s:|:t')
  mapField = L.over (rfield . L._Just)
  {-# INLINE mapField #-}

  -- | Shorthand to create a NamedField with a single field, using a DocField as
  -- an example.
  (=:) :: (ShowPath s) => DocField (s :|: a) -> a -> Rec field '[ s :|: a ]
  infixl 7 =:
  _ =: x = fromValue x :& RNil
  {-# INLINE (=:) #-}

  changePath :: (ShowPath s') => field (s:|:a) -> field (s':|:a)

type family FieldDirectlyContainsTag tag field where
  FieldDirectlyContainsTag tag (F.Compose (Tagged tag) f) = True
  FieldDirectlyContainsTag _ _ = False

-- | Extra argument to avoid overlapping instances
class (hasTag ~ FieldDirectlyContainsTag tag field) => FieldWithTag_ tag field hasTag where
  -- | Retrieves or modifies a tag (documentation, source...) within a field
  fieldTag :: L.Traversal' (field r) tag

-- | Tells whether 'fieldTag' can be used on a Field
type FieldWithTag tag field = FieldWithTag_ tag field (FieldDirectlyContainsTag tag field)

-- | Change the name of a field from the name of another
renamedAs :: (ShowPath s', NamedField f) => proxy (s':|:a) -> f (s:|:a) -> f (s':|:a)
renamedAs _ = changePath

instance NamedField Field where
  rfield f (Field v) = Field . fromM <$> f (Just v)
    where fromM (Just v') = v'
          fromM _         = error "Cannot remove a Field's value!"
  {-# INLINE rfield #-}
  fromValue = Field
  changePath (Field v) = Field v

instance (NamedField f) => NamedField (F.Compose PossiblyEmpty f) where
  rfield f (F.Compose (PE field)) = F.Compose . PE <$> inspect field
    where
      inspect (Right inner) = rebuild NoDefault (setInner inner) <$> f (L.view rfield inner)
      inspect (Left r)      = rebuild r fromValue <$> f Nothing
      setInner inner x = L.set rfield (Just x) inner
      rebuild reason _   Nothing  = Left reason
      rebuild _ updInner (Just x) = Right $ updInner x
  fromValue = F.Compose . PE . Right . fromValue
  changePath (F.Compose (PE x)) = F.Compose $ PE $ case x of
    Left r  -> Left r
    Right f -> Right $ changePath f

instance (FieldWithTag tag g)
  => FieldWithTag_ tag (F.Compose PossiblyEmpty g) False where
  fieldTag fn (F.Compose (PE (Right f))) =
    F.Compose . PE . Right <$> fieldTag fn f
  fieldTag _ field = pure field

-- | Tells the default tag to apply when creating a Field with 'fromValue'
class NamedFieldTag tag where
  -- | Tells the default tag to apply when creating a Field with 'fromValue'
  defaultTag :: tag
  -- | Permits to possibly keep the doc when setting a field
  tagFromDoc :: T.Text -> tag

instance NamedFieldTag T.Text where
  defaultTag = ""
  tagFromDoc = id

instance (NamedField f, NamedFieldTag tag) => NamedField (F.Compose (Tagged tag) f) where
  rfield = (\f (F.Compose (Tagged d x)) ->
               F.Compose . Tagged d <$> f x) . rfield
  {-# INLINE rfield #-}
  fromValue = F.Compose . Tagged defaultTag . fromValue
  changePath (F.Compose (Tagged d x)) = F.Compose (Tagged d (changePath x))
  -- | We redefine (=:) so as to keep the doc:
  F.Compose (Tagged d _) =: v = F.Compose (Tagged (tagFromDoc d) (fromValue v)) :& RNil

instance FieldWithTag_ tag Field False where
  fieldTag _ f = pure f

instance FieldWithTag_ tag (F.Compose (Tagged tag) f) True where
  fieldTag fn (F.Compose (Tagged d x)) = rebuild <$> fn d
    where
      rebuild d' = F.Compose (Tagged d' x)

instance (FieldWithTag tag f, FieldDirectlyContainsTag tag (F.Compose (Tagged tag') f) ~ False)
      => FieldWithTag_ tag (F.Compose (Tagged tag') f) False where
  fieldTag fn (F.Compose (Tagged t f)) = F.Compose . Tagged t <$> fieldTag fn f

-- | Turns a function (a -> b -> ... -> r) to (Field (s1:|:a) -> Field (s2:|:b)
-- -> ... r) so that it can be used with 'runcurry', 'runcurryA', etc.
class OnFields ts f1 f2 | ts f1 -> f2 where
  onFields :: f1 -> f2
instance OnFields '[] a a where
  onFields x = x
instance (OnFields ts f1 f2) =>
     OnFields ((s:|:a) : ts) (a -> f1) (Field (s:|:a) -> f2) where
  onFields f (Field x) = onFields @ts (f x)

runcurryF :: forall ts f1 f a. (OnFields ts f1 (CurriedF f ts a))
          => f1 -> Rec f ts -> a
runcurryF = runcurry . onFields @ts

runcurryAF :: forall ts f1 f g a. (Applicative f, OnFields ts f1 (CurriedF g ts a))
           => f1 -> Rec (F.Compose f g) ts -> f a
runcurryAF = runcurryA . onFields @ts

-- | Replaces RIndex from vinyl to show an explicit error message
type family RIndex' r1 rs1 (r :: k) (rs :: [k]) :: Nat where
  RIndex' r1 rs1 r (r ': rs) = 'Z
  RIndex' r1 rs1 r (notR ': rs) = 'S (RIndex' r1 rs1 r rs)
  RIndex' r1 rs1 r rs = TypeError
    (Text "Field " :<>: ShowType r1 :<>: Text " is not present in record " :<>: ShowType rs1)

-- | Tells whether rs contains Field f. It replaces vinyl's (∈) to provide
-- better error messages
class (RElem f rs (RIndex' f rs f rs)) => rs `HasField` f
instance (RElem f (r ': rs) (RIndex' f (r ': rs) f (r ': rs))) => (r ': rs) `HasField` f

-- | Replaces RImage from vinyl to show an explicit error message
-- type family RImage' rs1 ss1 (rs :: [k]) (ss :: [k]) :: [Nat] where
--   RImage' rs1 ss1 (r ': rs) ss = RIndex' rs1 ss1 r ss ': RImage' rs1 ss1 rs ss
--   RImage' rs1 ss1 '[] ss = TypeError

type family RImage' rs1 ss1 (rs :: [k]) (ss :: [k]) :: [Nat] where
  RImage' rs1 ss1 '[] ss = '[]
  RImage' rs1 ss1 (r ': rs) ss = RIndex' r ss r ss ': RImage' rs1 ss1 rs ss

-- | Tells whether rs contains Field f. It replaces vinyl's (⊆) to provide
-- better error messages
class (RSubset rs ss (RImage' rs ss rs ss)) => ss `Includes` rs
instance ss `Includes` '[]
instance (RSubset (r ': rs) ss (RImage' (r ': rs) ss (r ': rs) ss)) => ss `Includes` (r ': rs)

-- | Just a version of 'VL.rsubset' that uses the 'Includes' constraint, for
-- better error messages
rsubset :: (Functor g, ss `Includes` rs) => (Rec f rs -> g (Rec f rs)) -> Rec f ss -> g (Rec f ss)
rsubset = VL.rsubset

-- | Just a version of 'VL.rcast' that uses the 'Includes' constraint, for
-- better error messages
rcast :: (ss `Includes` rs) => Rec f ss -> Rec f rs
rcast = VL.rcast

-- | Just a version of 'VL.rcast' that uses the 'Includes' constraint, for
-- better error messages
rreplace :: (ss `Includes` rs) => Rec f rs -> Rec f ss -> Rec f ss
rreplace = VL.rreplace

-- | Replaces vinyl REquivalent to provide better error messages
type rs `EquivalentTo` ss = (rs `Includes` ss, ss `Includes` rs)

-- | Lens for getting a field's value inside some NamedField. Shortcut for
-- @rlens f . rfield@
fld :: forall s a rs field proxy.
       (NamedField field, rs `HasField` (s:|:a), ShowPath s)
    => proxy (s:|:a)
    -> L.Lens' (Rec field rs) (Maybe a)
fld _ = VL.rlens @(s:|:a) . rfield

-- | @r ^^. n@ is just a shortcut for @r ^. fld n . _Just@. Since the field can be empty
-- it requires it to be a Monoid
(^^.) :: (NamedField field, rs `HasField` (s:|:t), ShowPath s, Monoid t)
      => Rec field rs -> proxy (s:|:t) -> t
record ^^. field = record L.^. fld field . L._Just
infixl 8 ^^.

-- | @r ^^? n@ is just a shortcut for @r ^. fld n@
-- >>> let v2 = namedDefault & age %%~ (+1)
-- >>> v2^^?age
-- Just 13
--
(^^?) :: (NamedField field, rs `HasField` (s:|:t), ShowPath s)
      => Rec field rs -> proxy (s:|:t) -> (Maybe t)
record ^^? field = record L.^. fld field
infixl 8 ^^?

-- | @r ^^?! n@ is just a shortcut for @r ^?! fld n . L._Just@. It fails if
-- the field doesn't contain a value.
-- >>> let v2 = namedDefault & age %%~ (+1)
-- >>> v2^^?!age
-- 13
--
(^^?!) :: (NamedField field, rs `HasField` (s:|:t), ShowPath s)
       => Rec field rs -> proxy (s:|:t) -> t
record ^^?! field = record L.^?! fld field . L._Just
infixl 8 ^^?!

-- | @n %%~ f@ is just a shortcut for @fld n . _Just %~ f@. You can use it to set nested
-- records. For instance, @myPerson & parent%%~age..~30@ sets to 30 the age of
-- the parent in the object myPerson.
(%%~) :: (NamedField field, rs `HasField` (s:|:t), ShowPath s)
      => proxy (s:|:t) -> (t -> t) -> Rec field rs -> Rec field rs
field %%~ f = fld field . L._Just L.%~ f
infixr 4 %%~

-- | @n ..~ v@ is just a shortcut for @fld n .~ Just v@
--
-- >>> name ..~ "Bernard" $ defaultPerson
-- {age =: 12
-- , name =: "Bernard"
-- , size =: 130.0
-- }
(..~) :: (NamedField field, rs `HasField` (s:|:t), ShowPath s)
      => proxy (s:|:t) -> t -> Rec field rs -> Rec field rs
field ..~ v = fld field L..~ Just v
infixr 4 ..~

-- | A record with just an anonymous field. Useful when only the position of the
-- field is important
singleton :: (NamedField f) => t -> Rec f '[ ('[]:|:t ) ]
singleton x = fromValue x :& RNil

-- | Directly use a default value as part of a record. Will fail if @f@ doesn't
-- have a default value
useDef :: (NamedField f, ShowPath s) => DocField (s:|:t) -> Rec f '[ (s:|:t) ]
useDef f = f =: (f L.^?! rfield . L._Just)

-- | Used to create a field template
docField :: forall s t. (KnownSymbol s) => t -> T.Text -> DocField ('[s]:|:t)
docField defVal doc = DocField doc $ Right $ Field defVal

-- | Used to create an intermediary field
itmLevel
  :: forall s rs. (KnownSymbol s)
  => T.Text -> DocRec rs -> IntermediaryLevel '[s] rs
itmLevel doc content = ItmLvl $ DocField doc $ Right $ Field content

-- | Used to create a field from a default
fieldFromDef
  :: forall s t. (KnownSymbol s, Default t)
  => T.Text -> DocField ('[s]:|:t)
fieldFromDef = docField def

-- | Used to create a field that will not have a default value
fieldNoDef :: forall s t. T.Text -> DocField ('[s]:|:t)
fieldNoDef doc = DocField doc $ Left NoDefault

type family DeleteIn a b where
  DeleteIn t (t ': ts) = DeleteIn t ts
  DeleteIn t (t' ': ts) = t' : DeleteIn t ts
  DeleteIn t '[] = '[]

type family Difference a b where
  Difference ts' (t ': ts) = Difference (DeleteIn t ts') ts
  Difference ts' '[] = ts'

-- | Splits a record in two parts by using an existing record type.
rcastAs
  :: (rs `Includes` selected)
  => p selected -> Rec f rs -> Rec f selected
rcastAs _ r = rcast r

-- | Splits a record in two parts.
rsplit
  :: (rs `Includes` selected, rs `Includes` (rs `Difference` selected))
  => Rec f rs -> (Rec f selected, Rec f (rs `Difference` selected))
rsplit r = (rcast r, rcast r)

-- | Splits a record in two parts by using an existing record type.
rsplitFrom
  :: (rs `Includes` selected, rs `Includes` (rs `Difference` selected))
  => p selected -> Rec f rs -> (Rec f selected, Rec f (rs `Difference` selected))
rsplitFrom _ r = rsplit r

-- | "Subtracts" one record from another. In other term, splits a record in two
-- parts by selecting the fields from an existing record
rdifference
  :: (rs `Includes` selected, rs `Includes` (rs `Difference` selected))
  => Rec f rs -> p selected -> Rec f (rs `Difference` selected)
rdifference r _ = rcast r

type a `Intersection` b = a `Difference` (a `Difference` b)

-- | Returns (fields only in a, values in a of fields in both, values in b of
-- fields in both, fields only in b)
rintersection
  :: (a `Includes` (a `Difference` b)
     ,a `Includes` (a `Intersection` b)
     ,b `Includes` (b `Intersection` a)
     ,b `Includes` (b `Difference` a))
  => Rec f a -> Rec f b -> ( Rec f (a `Difference` b), Rec f (a `Intersection` b)
                           , Rec f (b `Intersection` a), Rec f (b `Difference` a) )
rintersection a b = (rcast a, rcast a, rcast b, rcast b)

class PrefixPath (s::[Symbol]) rs where
  type s `PrefixingAll` rs :: [PathWithType [Symbol] *]
  prefixPath :: (NamedField f) => Rec f rs -> Rec f (s `PrefixingAll` rs)
instance PrefixPath s '[] where
  type s `PrefixingAll` '[] = '[]
  prefixPath _ = RNil
instance (PrefixPath s ps, ShowPath (s++p1)) => PrefixPath s ( (p1:|:t) : ps) where
  type s `PrefixingAll` ( (p1:|:t) : ps) = ( (s++p1:|:t) : s `PrefixingAll` ps)
  prefixPath (f :& fs) = (changePath f) :& prefixPath @s fs

-- | Used to indicate that a field contains no useful value, only metadata (doc)
data MD = MD

instance Eq MD where
  _ == _ = True
instance Ord MD where
  compare _ _ = EQ
instance ToJSON MD where
  toJSON _ = Null
instance FromJSON MD where
  parseJSON _ = pure MD
instance Show MD where
  show _ = "<just_doc>"

newtype IntermediaryLevel_ a = ItmLvl (DocField a)

-- | Used to indicate "virtual" fields, that won't be directly filled with data
-- but will be used by 'rinclude', 'rdrill', '(-.)' and '(-/)' to pinpoint a
-- subrecord in the hierarchy and indicate what this subrecord is meant to
-- contain
type IntermediaryLevel s rs = IntermediaryLevel_ (s:|:DocRec rs)

-- | Transforming the type of an IntermediaryLevel into a regular record
type FlattenedLevel s rs = s `PrefixingAll` rs

-- | Flatten a field of records into a record by altering the path of each
-- subfield
rinclude
  :: forall s rs. (PrefixPath s rs, ShowPath s)
  => IntermediaryLevel s rs -> DocRec (FlattenedLevel s rs)
rinclude (ItmLvl (DocField _ (Left r))) =
  error $ "rinclude: Trying to flatten an empty field (" ++ show r ++ ")!"
rinclude (ItmLvl (DocField _doc (Right (Field r)))) = prefixPath @s r

-- | Is a class so we can compose both 'IntermediaryLevel's and 'DocField's
class ComposableNesting f lvl2 where
  -- | Appends together two fields in a nested fashion. Will build either a
  -- final DocField or another IntermediaryLevel, depending on the second
  -- argument.
  (-.) :: (NestedLvlConstraints rs f p lvl2, ShowPath (s++p))
       => IntermediaryLevel s rs -> f (p:|:lvl2) -> NestedLvl s f p lvl2
  infixr 9 -.

type family NestedLvl s f p lvl2 where
  NestedLvl s IntermediaryLevel_ p (DocRec rs') = IntermediaryLevel (s++p) rs'
  NestedLvl s f p t = f ((s++p):|:t)

type family NestedLvlConstraints rs f p lvl2 :: Constraint where
  NestedLvlConstraints rs IntermediaryLevel_ p (DocRec rs') =
    ( rs `Includes` (p `PrefixingAll` rs') )
  NestedLvlConstraints rs f p t =
    ( rs `HasField` (p:|:t) )

instance ComposableNesting IntermediaryLevel_ (DocRec rs') where
  _ -. ItmLvl f = ItmLvl $ changePath f

instance (NamedField f) => ComposableNesting f t where
  _ -. f = changePath f

-- | A version of '(-.)' for when you don't have an 'IntermediaryLevel' to use
-- as prefix and just want a single-symbol prefix
funder :: forall s p t. ( ShowPath (s ': p) )
     => DocField (p:|:t) -> DocField ((s ': p) :|: t)
funder = changePath

-- | A version of '(-.)' for altering the paths of a whole record at once
(-/) :: forall s rs selected f.
        (rs `Includes` selected, PrefixPath s selected, NamedField f)
     => IntermediaryLevel s rs -> Rec f selected -> Rec f (s `PrefixingAll` selected)
_ -/ r = prefixPath @s r
infixr 6 -/

-- | A version of '(-/)' for when you don't have an 'IntermediaryLevel' to use
-- as prefix and just want a single-symbol prefix
runder :: forall s selected f. (PrefixPath '[s] selected, NamedField f)
      => Rec f selected -> Rec f ('[s] `PrefixingAll` selected)
runder r = prefixPath @'[s] r

type family Strip s src where
  Strip (a ': as) (a ': bs) = Strip as bs
  Strip (a ': as) (b ': bs) = b ': bs
  Strip as bs = bs

class UnprefixPath (s::[Symbol]) rs where
  type s `UnprefixingAll` rs :: [PathWithType [Symbol] *]
  unprefixPath :: (NamedField f) => Rec f rs -> Rec f (s `UnprefixingAll` rs)
instance UnprefixPath s '[] where
  type s `UnprefixingAll` '[] = '[]
  unprefixPath _ = RNil
instance (UnprefixPath s ps, ShowPath (Strip s p1))
       => UnprefixPath s ( (p1:|:t) : ps) where
  type s `UnprefixingAll` ( (p1:|:t) : ps) =
          ( (Strip s p1 :|: t) : s `UnprefixingAll` ps)
  unprefixPath (f :& fs) = changePath f :& unprefixPath @s fs

-- | Selects a subrecord from a record @r@, using an 'IntermediaryLevel'. (This
-- 'IntermediaryLevel' has normally originally been passed to 'rinclude' to
-- obtain @r@)
rdrill :: forall s inner outer f.
          ( inner ~ (s `UnprefixingAll` (s `PrefixingAll` inner))
              -- This is always the case, but GHC doesn't have the proof of that
          , UnprefixPath s (s `PrefixingAll` inner)
          , outer `Includes` (s `PrefixingAll` inner)
          , NamedField f )
       => IntermediaryLevel s inner
       -> Rec f outer
       -> Rec f inner
rdrill _ outer = unprefixPath @s (rcast outer :: Rec f (s `PrefixingAll` inner))

-- | Combines a drill and a split
rsplitDrill :: forall s inner outer f.
          ( inner ~ (s `UnprefixingAll` (s `PrefixingAll` inner))
              -- This is always the case, but GHC doesn't have the proof of that
          , UnprefixPath s (s `PrefixingAll` inner)
          , outer `Includes` (s `PrefixingAll` inner)
          , outer `Includes` (outer `Difference` (FlattenedLevel s inner))
          , NamedField f )
       => IntermediaryLevel s inner
       -> Rec f outer
       -> (Rec f inner, Rec f (outer `Difference` (FlattenedLevel s inner)))
rsplitDrill il outer = (rdrill il outer, rcast outer)

-- | Merges a whole subset of the tree to a single field
rfoldSubset
  :: forall outer' inner outer p t proxy f.
  ( outer `Includes` inner
  , ((p:|:t) ': outer) `Includes` outer')
  => proxy inner  -- ^ The list of fields to target
  -> (Rec f inner -> f (p:|:t))
  -> Rec f outer
  -> Rec f outer'
rfoldSubset _ f r = rcast $ f (rcast r) :& r

-- | Just a helper to fix some types
withSameFields :: Rec f rs -> Rec g rs -> t -> t
withSameFields _ _ x = x

type IdentityField = F.Identity

-- | Just a shortcut to build identity records (i.e. simple heterogeneous
-- lists. Useful for applying different function over different fields of a
-- record with 'ApplyRec'
(&:) :: t -> Rec IdentityField ts -> Rec IdentityField (t ': ts)
x &: r = F.Identity x :& r
infixr 5 &:

-- | Applies a record of functions to a record of data. It's a bit like the
-- (<<*>>) operator from vinyl but it permits to change the type of the fields,
-- which (<<$>>) from vinyl doesn't.
class ApplyRec fns fields results | fns fields -> results where
  appRec :: (NamedField f) => Rec F.Identity fns -> Rec f fields -> Rec f results

instance ApplyRec '[] a a where
  appRec RNil r = r

instance (ApplyRec fns fields results, ShowPath s)
  => ApplyRec ( (a -> b) : fns ) ((s:|:a) : fields) ((s:|:b) : results) where
  appRec (F.Identity f :& fns) (field :& fields) =
    L.over (rfield . L._Just) f field :& appRec fns fields

-- | Whether the first field of a record should be ignored when constructing it
type family FirstFieldSkipped rs where
  FirstFieldSkipped ((s:|:MD) : rs) = 'True
  FirstFieldSkipped a  = 'False

class (skipFirst ~ FirstFieldSkipped rs)
   => BuildRecFrom f rs (acc::[PathWithType [Symbol] *]) skipFirst where
  type RecCtor f rs acc skipFirst
  buildRecFrom_ :: Rec f acc -> DocRec rs -> RecCtor f rs acc skipFirst

instance BuildRecFrom f '[] acc 'False where
  type RecCtor f '[] acc 'False = Rec f acc
  buildRecFrom_ acc RNil = acc
  {-# INLINE buildRecFrom_ #-}

instance ( BuildRecFrom f rs (acc ++ '[s:|:a]) (FirstFieldSkipped rs)
         , FirstFieldSkipped ((s:|:a):rs) ~ 'False
         , NamedField f, ShowPath s )
      => BuildRecFrom f ((s:|:a) : rs) acc 'False where
  type RecCtor f ((s:|:a) : rs) acc 'False =
         a -> RecCtor f rs (acc ++ '[s:|:a]) (FirstFieldSkipped rs)
  buildRecFrom_ acc (r :& rs) a = buildRecFrom_ (acc <+> r =: a) rs
   -- The append at the of the record makes it quadratic in comlexity. It's not
   -- great, it could me made to be linear.
  {-# INLINE buildRecFrom_ #-}

instance ( BuildRecFrom f rs (acc++'[s:|:MD]) (FirstFieldSkipped rs)
         , NamedField f, ShowPath s )
      => BuildRecFrom f ((s:|:MD) : rs) acc 'True where
  type RecCtor f ((s:|:MD) : rs) acc 'True =
         RecCtor f rs (acc ++ '[s:|:MD]) (FirstFieldSkipped rs)
  buildRecFrom_ acc (_ :& rs) = buildRecFrom_ (acc <+> fromValue @f @s MD :& RNil) rs
  {-# INLINE buildRecFrom_ #-}

-- | Generic construct for records. It takes as many arguments as the example
-- DocRec contains fields, except for MD fields which are skipped.
recFrom :: forall f rs. (BuildRecFrom f rs '[] (FirstFieldSkipped rs))
        => DocRec rs -> RecCtor f rs '[] (FirstFieldSkipped rs)
recFrom = buildRecFrom_ @f RNil
{-# INLINE recFrom #-}

-- | Transforms a 'Rec f as' into a 'Rec f bs' and the other way around. This is
-- exactly like an Iso from Lens, but using an Iso makes it harder to implement
-- (<<|>>). Maybe in the future we'll get back to regular Lenses and Isos
-- (because this way composition of Isos and Lenses together is done for us and
-- behaves sanely. Plus we get plain old function composition instead of having
-- to import Control.Category).
-- This could be done by making bijectField/addConstField etc
data RecBijection f as bs = RecBijection
  { applyRecBij    :: Rec f as -> Rec f bs
  , applyRecBijInv :: Rec f bs -> Rec f as
  }

instance Cat.Category (RecBijection f) where
  id = RecBijection id id
  RecBijection f fi . RecBijection g gi = RecBijection (f . g) (gi . fi)

-- | Returns the inverse of the bijection
invertRecBij :: RecBijection f as bs -> RecBijection f bs as
invertRecBij (RecBijection f g) = RecBijection g f

-- | Composes two 'RecBijection's in a parallel fashion.
(<<|>>)
  :: ( as `Intersection` as' ~ '[]
     , bs `Intersection` bs' ~ '[]
     , (as ++ as') `Includes` as, (as ++ as') `Includes` as'
     , (bs ++ bs') `Includes` bs, (bs ++ bs') `Includes` bs')
  => RecBijection f as bs
  -> RecBijection f as' bs'
  -> RecBijection f (as++as') (bs++bs')
RecBijection f fi <<|>> RecBijection g gi =
  RecBijection (\r -> f (rcast r) <+> g (rcast r))
               (\r -> fi (rcast r) <+> gi (rcast r))

-- | Creates a 'RecBijection' that just maps over a singleton 'Rec'
bijectField :: forall s f a b. (ShowPath s, NamedField f)
               => (a -> b) -> (b -> a) -> RecBijection f '[s:|:a] '[s:|:b]
bijectField f g = RecBijection (\(fl :& RNil) -> mapField f fl :& RNil)
                                    (\(fl :& RNil) -> mapField g fl :& RNil)
{-# INLINE bijectField #-}

-- | Creates a 'RecBijection' that just maps over a singleton 'Rec' and changes the name along
bijectField' :: forall s s' f a b. (ShowPath s, ShowPath s', NamedField f)
               => (a -> b) -> (b -> a) -> RecBijection f '[s:|:a] '[s':|:b]
bijectField' f g = RecBijection (\(fl :& RNil) -> changePath (mapField f fl) :& RNil)
                                    (\(fl :& RNil) -> changePath (mapField g fl) :& RNil)
{-# INLINE bijectField' #-}

-- | Creates a 'RecBijection' that changes the path of the field in a singleton
-- 'Rec'
renameField :: forall s s' f a. (ShowPath s, ShowPath s', NamedField f)
              => RecBijection f '[s:|:a] '[s':|:a]
renameField = RecBijection (\(fl :& RNil) -> changePath fl :& RNil)
                             (\(fl :& RNil) -> changePath fl :& RNil)
{-# INLINE renameField #-}

-- | Just adds a field that will be constant
addConstField :: forall s f a. f (s:|:a) -> RecBijection f '[] '[s:|:a]
addConstField x = RecBijection (\_ -> x :& RNil) (const RNil)
{-# INLINE addConstField #-}

-- | A version of '(-/)' for 'RecBijection's
bijectUnder
  :: forall s f as bs.
     ( as ~ UnprefixingAll s (PrefixingAll s as)
     , bs ~ UnprefixingAll s (PrefixingAll s bs)
     , PrefixPath s as, PrefixPath s bs, NamedField f
     , UnprefixPath s (PrefixingAll s as)
     , UnprefixPath s (PrefixingAll s bs) )
  => RecBijection f as bs
  -> RecBijection f (s `PrefixingAll` as) (s `PrefixingAll` bs)
bijectUnder (RecBijection f fi) =
  RecBijection (prefixPath @s . f . unprefixPath @s)
               (prefixPath @s . fi . unprefixPath @s)
