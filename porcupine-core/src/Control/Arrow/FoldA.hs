{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | This module defines the type 'FoldA', which is a generalization of 'Fold'
-- from the `foldl` package.

module Control.Arrow.FoldA
  ( module Control.Foldl
  , FoldA(..)
  , Pair(..)
  , generalizeA
  , specializeA
  , premapA
  , postmapA
  , prefilterA

  , toTup
  , fromTup
  , (&&&&)
  , (****)
  ) where

import           Prelude          hiding (id, (.))

import           Control.Arrow
import           Control.Category
import           Control.Foldl
import           Data.Profunctor


ret :: (Arrow arr) => t -> arr a t
ret = arr . const
{-# INLINE ret #-}

data Pair a b = Pair !a !b

toTup :: Pair a b -> (a,b)
toTup (Pair x y) = (x,y)
{-# INLINE toTup #-}

fromTup :: (a,b) -> Pair a b
fromTup (x,y) = Pair x y
{-# INLINE fromTup #-}

applyP :: Pair (a -> b) a -> b
applyP (Pair f x) = f x
{-# INLINE applyP #-}

uncurryP :: (a -> b -> c) -> Pair a b -> c
uncurryP f (Pair x y) = f x y
{-# INLINE uncurryP #-}

(****) :: (Arrow a) => a b c -> a b' c' -> a (Pair b b') (Pair c c')
a1 **** a2 =
  arr toTup >>> (a1 *** a2) >>> arr fromTup
{-# INLINE (****) #-}

(&&&&) :: (Arrow a) => a b c -> a b c' -> a b (Pair c c')
a1 &&&& a2 = a1 &&& a2 >>> arr fromTup
{-# INLINE (&&&&) #-}

secondP :: (Arrow a) => a c c' -> a (Pair b c) (Pair b c')
secondP ar =
  arr toTup >>> second ar >>> arr fromTup

-- | If arr is (->), then 'FoldA' is just 'Fold'
data FoldA arr a b =
  forall x. FoldA (arr (Pair x a) x) (arr () x) (arr x b)

generalizeA :: (Arrow arr) => Fold a b -> FoldA arr a b
generalizeA (Fold step start done) =
  FoldA (arr $ uncurryP step) (ret start) (arr done)

specializeA :: FoldA (->) a b -> Fold a b
specializeA (FoldA step start done) =
  Fold (\x a -> step $ Pair x a) (start ()) done

instance (Arrow arr) => Functor (FoldA arr a) where
  fmap f (FoldA step start done) = FoldA step start done'
    where
      done' = done >>> arr (f $!)
  {-# INLINE fmap #-}

instance (Arrow arr) => Applicative (FoldA arr a) where
  pure x = FoldA (ret ()) (ret ()) (ret x)
  {-# INLINE pure #-}

  FoldA stepL startL doneL <*> FoldA stepR startR doneR =
    let step =
          arr (\(Pair (Pair xL xR) a) ->
                 Pair (Pair xL a) (Pair xR a))
          >>> (stepL **** stepR)
        start = startL &&&& startR
        done = (doneL **** doneR) >>> arr applyP
    in FoldA step start done
  {-# INLINE (<*>) #-}

instance (Arrow arr) => Profunctor (FoldA arr) where
  rmap = fmap
  lmap f (FoldA step start done) = FoldA step' start done
    where
      step' = arr (\(Pair x a) -> Pair x (f a)) >>> step
  {-# INLINE lmap #-}

premapA :: (Arrow arr)
        => arr a b -> FoldA arr b r -> FoldA arr a r
premapA ar (FoldA step start done) =
  FoldA (secondP ar >>> step) start done
{-# INLINABLE premapA #-}

postmapA :: (Category arr)
         => FoldA arr a b -> arr b r -> FoldA arr a r
postmapA (FoldA step start done) ar =
  FoldA step start (done >>> ar)
{-# INLINABLE postmapA #-}

prefilterA :: (ArrowChoice arr)
           => arr a Bool -> FoldA arr a r -> FoldA arr a r
prefilterA fltr (FoldA step start done) =
  FoldA (proc (Pair x a) -> do
            b <- fltr -< a
            if b
              then step -< Pair x a
              else returnA -< x)
        start
        done
{-# INLINABLE prefilterA #-}
