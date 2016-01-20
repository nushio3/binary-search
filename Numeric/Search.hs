{-# LANGUAGE FlexibleContexts #-}

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
-- Just 1.732
--
-- >>> import Data.SBV
-- >>> let x ⊂ r = (2 .<= x &&& x .< r-2)
-- >>> let x ∅ y = (abs (x-y) .>=4)
-- >>> let contain3 r = \x y z -> bAnd [x ⊂ r, y ⊂ r, z ⊂ r]

module Numeric.Search (
         -- * Evidence
         Evidence(..),
         -- * Search Range
         Range,
         SearchRange,
         -- * Splitters
         divForever, divTill,
         divideForever, divideTill,

         -- * Search
         search, searchM,
         -- * Postprocess
         smallest, largest

) where

import Numeric.Search.Combinator.Pure
import Numeric.Search.Combinator.Monadic

-- $setup
-- >>> :set -XFlexibleContexts
