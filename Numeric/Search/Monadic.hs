-- | This module defines the most generalized form of binary search.
-- Namely, it's the binary search over predicate of the form @('Eq' b,
-- 'Monad' m) => a -> m b@ . The other searches are derived as special
-- cases of this function.
--
-- 'BinarySearch' assumes two things;
--
-- 1. @b@, the codomain of 'Predicate' belongs to type class 'Eq'.
--
-- 2. Each value of @b@ form a convex set in the codomain space of the
-- Predicate. That is, if for certain pair @(left, right) :: (a, a)@
-- satisfies @pred left == val && pred right == val@, then also
-- @pred x == val@ for all @x@ such that @left <= x <= right@ .

{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Search.Monadic where

import           Data.Foldable (foldrM)
import           Data.Sequence as Seq
import           Prelude hiding (init, pred)

-- | The generalized type for binary search functions.
type BinarySearch m a b =
  Initializer m a b ->
  Cutter m a ->
  Predicate m a b ->
  m (Seq (BookEnd a b))

-- | 'BookEnd' comes in order [LEnd, REnd, LEnd, REnd ...], and
-- represents the ongoing state of the search results.
-- Two successive 'BookEnd' @LEnd x1 b1@, @REnd x2 b1@ represents a
-- claim that @pred x == b1@ for all @x@ such that @x1 <= x <= x2@ .
data BookEnd a b
      = LEnd !a !b
      | REnd !a !b

-- | 'Predicate' @m a b@ calculates the predicate in the context @m@.
type Predicate m a b = a -> m b

-- | 'Initializer' generates the initial set of ranges.
type Initializer m a b = Predicate m a b -> m (Seq (BookEnd a b))

-- | 'Cutter' @x1 x2@ decides if we should further investigate the
-- gap between @x1@ and @x2@. If so, it gives a new value @x3@ wrapped
-- in a 'Just'.
type Cutter m a = a -> a -> m (Maybe a)

searchWith :: forall m a b. (Monad m) => BinarySearch m a b
searchWith init cut pred = do
  seq0 <- init pred
  go seq0
  where
    go :: Seq (BookEnd a b) -> m (Seq (BookEnd a b))
    go seq = foldrM accum Seq.empty seq

    accum :: BookEnd a b -> Seq (BookEnd a b) -> m (Seq (BookEnd a b))
    accum x seq = case Seq.veiwl seq of
      EmptyL x
      | Seq.null seq = return $ Seq.singleton r
      | otherwise    = return $ r <| seq