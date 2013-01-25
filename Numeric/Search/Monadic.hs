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

import           Control.Applicative((<$>))
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
      deriving (Eq, Show)

-- | 'Predicate' @m a b@ calculates the predicate in the context @m@.
type Predicate m a b = a -> m b

-- | 'Initializer' generates the initial set of ranges.
type Initializer m a b = Predicate m a b -> m (Seq (BookEnd a b))

-- | 'Cutter' @x1 x2@ decides if we should further investigate the
-- gap between @x1@ and @x2@. If so, it gives a new value @x3@ wrapped
-- in a 'Just'.
type Cutter m a = a -> a -> m (Maybe a)

searchWith :: forall m a b. (Functor m, Monad m, Eq b) => BinarySearch m a b
searchWith init cut pred = do
  seq0 <- init pred
  go seq0
  where
    go :: Seq (BookEnd a b) -> m (Seq (BookEnd a b))
    go seq0 = case viewl seq0 of
      EmptyL -> return seq0
      (x1 :< seq1) -> do
        let skip = (x1 <|) <$> go seq1
        case viewl seq1 of
          EmptyL -> skip
          (x2 :< seq2) -> case (x1,x2) of
            (REnd a1 b1, LEnd a2 b2) -> do
              y1 <- drillDown a1 b1 a2 b2
              y2 <- go seq2
              return $ y1 >< y2
            _ -> skip

    drillDown :: a -> b -> a -> b -> m (Seq (BookEnd a b))
    drillDown a1 b1 a2 b2= do
      mc <- cut a1 a2
      case mc of
        Nothing -> return $ Seq.fromList [REnd a1 b1, LEnd a2 b2]
        Just a3 -> do
          b3 <- pred a3
          case () of
            _ | b3==b1 -> drillDown a3 b3 a2 b2
            _ | b3==b2 -> drillDown a1 b1 a3 b3
            _ -> do
              y1 <-  drillDown a1 b1 a3 b3
              y2 <-  drillDown a3 b3 a2 b2
              return $ y1 >< y2