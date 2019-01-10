{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | This module defines the type 'FoldA', which is a generalization of 'Fold'
-- from the `foldl` package.

module Control.Arrow.FoldA
  ( module Control.Foldl
  , FoldA(..)
  , FoldA'
  , Pair(..)
  , generalizeA
  , specializeA
  , premapA
  , premapInitA
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

-- | This is a generalization of 'Control.Foldl.Fold' that
-- allows computing on arrows.
--
-- 'FoldA (->) ()' is isomorphic to 'Fold' as testified
-- by 'generalizeA' and 'specializeA'.
--
-- 'FoldA (Kleisly m) ()' is isomorphic to 'FoldM m'.
--
data FoldA arr i a b =
  forall x. FoldA (arr (Pair x a) x) (arr i x) (arr x b)

-- | A fold that will directly receive its initial accumulator
type FoldA' arr a b = FoldA arr b a b

generalizeA :: (Arrow arr) => Fold a b -> FoldA arr i a b
generalizeA (Fold step start done) =
  FoldA (arr $ uncurryP step) (ret start) (arr done)

specializeA :: FoldA (->) () a b -> Fold a b
specializeA (FoldA step start done) =
  Fold (\x a -> step $ Pair x a) (start ()) done

instance (Arrow arr) => Functor (FoldA arr i a) where
  fmap f (FoldA step start done) = FoldA step start done'
    where
      done' = done >>> arr (f $!)
  {-# INLINE fmap #-}

instance (Arrow arr) => Applicative (FoldA arr i a) where
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

instance (Arrow arr) => Profunctor (FoldA arr i) where
  rmap = fmap
  lmap f (FoldA step start done) = FoldA step' start done
    where
      step' = arr (\(Pair x a) -> Pair x (f a)) >>> step
  {-# INLINE lmap #-}

-- | Changes the type initializing the accumulator
premapInitA :: (Arrow arr)
            => arr i' i -> FoldA arr i a b -> FoldA arr i' a b
premapInitA ar (FoldA step start done) =
  FoldA step (ar >>> start) done

-- | Changes all the inputs arriving to the 'FoldA'
premapA :: (Arrow arr)
        => arr a b -> FoldA arr i b r -> FoldA arr i a r
premapA ar (FoldA step start done) =
  FoldA (secondP ar >>> step) start done
{-# INLINABLE premapA #-}

-- | Changes the output of the 'FoldA'
postmapA :: (Category arr)
         => FoldA arr i a b -> arr b r -> FoldA arr i a r
postmapA (FoldA step start done) ar =
  FoldA step start (done >>> ar)
{-# INLINABLE postmapA #-}

prefilterA :: (ArrowChoice arr)
           => arr a Bool -> FoldA arr i a r -> FoldA arr i a r
prefilterA fltr (FoldA step start done) =
  FoldA (proc (Pair x a) -> do
            b <- fltr -< a
            if b
              then step -< Pair x a
              else returnA -< x)
        start
        done
{-# INLINABLE prefilterA #-}
