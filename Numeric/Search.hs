{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, MultiWayIf, RecordWildCards, ScopedTypeVariables, TupleSections #-}

-- | This package provides combinators to construct many variants of
-- binary search.  Most generally, it provides the binary search over
-- predicate of the form @('Eq' b, 'Monad' m) => a -> m b@ . The other
-- searches are derived as special cases of this function.
--
-- 'BinarySearch' assumes two things;
--
-- 1. @b@, the codomain of 'PredicateM' belongs to type class 'Eq'.
--
-- 2. Each value of @b@ form a convex set in the codomain space of the
-- PredicateM. That is, if for certain pair @(left, right) :: (a, a)@
-- satisfies @pred left == val && pred right == val@, then also @pred
-- x == val@ for all @x@ such that @left <= x <= right@ .
--
-- __Example 1.__ Find the approximate square root of 3.
--
-- >>> largest True $ search positiveExponential divForever (\x -> x^2 < 3000000)
-- Just 1732
-- >>> smallest False $ search positiveExponential divForever (\x -> x^2 < 3000000)
-- Just 1733
-- >>> largest True $ search positiveExponential divideForever (\x -> x^2 < (3::Double))
-- Just 1.7320508075688772
--
-- >>> import Data.SBV
-- >>> let x ⊂ r = (2 .<= x &&& x .< r-2)
-- >>> let x ∅ y = (abs (x-y) .>=4)
-- >>> let contain3 r = \x y z -> bAnd [x ⊂ r, y ⊂ r, z ⊂ r]

module Numeric.Search where

import           Control.Applicative((<$>))
import           Data.Functor.Identity
import           Prelude hiding (init, pred)


-- $setup
-- >>> :set -XFlexibleContexts

-- * Evidence

-- | The 'Evidence' datatype is similar to 'Either' , but differes in that all 'CounterEvidence' values are
--   equal to each other, and all 'Evidence' values are also
--   equal to each other. The 'Evidence' type is used to binary-searching for some predicate and meanwhile returning evidences for that.
--
-- In other words, 'Evidence' is a 'Bool' with additional information why it is 'True' or 'False'.
--
-- >>> Evidence "He owns the gun" == Evidence "He was at the scene"
-- True
-- >>> Evidence "He loved her" == CounterEvidence "He loved her"
-- False

data Evidence a b = CounterEvidence a | Evidence b
                  deriving (Show, Read, Functor)

instance Eq (Evidence b a) where
  CounterEvidence _ == CounterEvidence _ = True
  Evidence _        == Evidence _        = True
  _                 == _                 = False

instance Ord (Evidence b a) where
  CounterEvidence _ `compare` CounterEvidence _ = EQ
  Evidence _        `compare` Evidence _        = EQ
  CounterEvidence _ `compare` Evidence _        = GT
  Evidence _        `compare` CounterEvidence _ = LT

instance Applicative (Evidence e) where
    pure                     = Evidence
    CounterEvidence  e <*> _ = CounterEvidence e
    Evidence f <*> r         = fmap f r

instance Monad (Evidence e) where
    return                   = Evidence
    CounterEvidence  l >>= _ = CounterEvidence l
    Evidence r >>= k         = k r

-- * Search range


-- | The @Range k lo  k' hi@ represents the search result that @pred x == k@ for @lo <= x <= hi@.
-- The 'Range' type also holds the evidences for the lower and the upper boundary.

data Range b a = Range {loKey :: b, loVal :: a, hiKey :: b, hiVal :: a}


-- | The lists of candidate for lower and upper bounds from which the search may be started.
type SearchRange a = ([a], [a])


-- | Set the lower and upper boundary from those available from the candidate lists.
-- From the pair of list, the @initializeSearchM@ tries to find the first @(lo, hi)@
-- such that @pred lo /= pred hi@, by the breadth-first search.

initializeSearchM :: (Monad m, Eq b)=> SearchRange a -> (a -> m b) -> m [Range b a]
initializeSearchM (lo:los,hi:his) pred0 = do
  pLo <- pred0 lo
  pHi <- pred0 hi
  let
    pop (p,x, []) = return (p,x,[])
    pop (p,_, x2:xs) = do
      p2 <- pred0 x2
      return (p2, x2, xs)

    go pez1@(p1,x1,xs1) pez2@(p2,x2,xs2)
        | p1 /= p2             = return [Range p1 x1 p1 x1, Range p2 x2 p2 x2]
        | null xs1 && null xs2 = return [Range p1 x1 p2 x2]
        | otherwise = do
            pez1' <- pop pez1
            pez2' <- pop pez2
            go pez1' pez2'

  go (pLo, lo,los) (pHi, hi, his)
initializeSearchM _ _ = return []


-- | Search between 'minBound' and 'maxBound' .
minToMax :: Bounded a => SearchRange a
minToMax = ([minBound], [maxBound])


exponential :: Num a => SearchRange a
exponential = (iterate (*2) (-1), 0 : iterate (*2) 1)

positiveExponential :: Num a => SearchRange a
positiveExponential = ([1], iterate (*2) 2)

nonNegativeExponential :: Num a => SearchRange a
nonNegativeExponential = ([0], iterate (*2) 1)

negativeExponential :: Num a => SearchRange a
negativeExponential = (iterate (*2) (-2), [-1])

nonPositiveExponential :: Num a => SearchRange a
nonPositiveExponential = (iterate (*2) (-1), [0])



-- * Splitters

type Splitter a = a -> a -> Maybe a

-- | Perform split forever, until we cannot find a mid-value due to machine precision.
-- This splitter uses the `div` funtion.
divForever :: Integral a => Splitter a
divForever lo hi = let mid = lo `div` 2 + hi `div` 2 in
  if lo == mid || mid == hi then Nothing
  else Just mid

-- | Perform splitting until @hi - lo <= eps@ .
divTill :: Integral a => a -> Splitter a
divTill eps lo hi
  | hi - lo <= eps = Nothing
  | otherwise      = divForever lo hi

-- | Perform split forever, until we cannot find a mid-value due to machine precision.
-- This one uses `(/)` operator.
divideForever :: (Eq a,Fractional a) => Splitter a
divideForever lo hi = let mid = lo / 2 + hi / 2 in
  if lo == mid || mid == hi then Nothing
  else Just mid

-- | Perform splitting until @hi - lo <= eps@ .
divideTill :: (Ord a, Fractional a) => a -> Splitter a
divideTill eps lo hi
  | hi - lo <= eps = Nothing
  | otherwise      = divideForever lo hi


-- * Searching

-- | Perform search over pure predicates. The monadic version of this is 'searchM' .
search :: (Eq b) =>
          SearchRange a -> Splitter a -> (a -> b) -> [Range b a]
search init0 split0 pred0 = runIdentity $ searchM init0 split0 (Identity . pred0)


-- | Mother of all search variations.
--
-- 'searchM' keeps track of the predicates found, so that it works well with the 'Evidence' type.

searchM :: forall a m b . (Monad m, Eq b) =>
           SearchRange a -> Splitter a -> (a -> m b) -> m [Range b a]
searchM init0 split0 pred0 = do
  ranges0 <- initializeSearchM init0 pred0
  go ranges0
    where
      go :: [Range b a] -> m [Range b a]
      go (r1@(Range p0 lo1 p1 hi1):r2@(Range p2 lo2 p3 hi2):rest) = case split0 hi1 lo2 of
        Nothing   -> (r1:) <$> go (r2:rest)
        Just mid1 -> do
          pMid <- pred0 mid1
          if | p1 == pMid -> go $ (Range p0 lo1 pMid mid1) : r2 : rest
             | p2 == pMid -> go $ r1 : (Range pMid mid1 p3 hi2) : rest
             | otherwise  -> go $ r1 : (Range pMid mid1 pMid mid1) : r2 : rest
      go xs = return xs

-- * Postprocess

lookupRanges :: (Eq b) => b -> [Range b a] -> Maybe (Range b a)
lookupRanges k [] = Nothing
lookupRanges k (r@Range{..}:rs)
  | loKey == k  = Just r
  | otherwise   = lookupRanges k rs

-- | Pick up the smallest @a@ that satisfies @pred a == b@ .
smallest :: (Eq b) => b -> [Range b a] -> Maybe a
smallest b rs = loVal <$> lookupRanges b rs

-- | Pick up the largest @a@ that satisfies @pred a == b@ .
largest :: (Eq b) => b -> [Range b a] -> Maybe a
largest b rs = hiVal <$> lookupRanges b rs
