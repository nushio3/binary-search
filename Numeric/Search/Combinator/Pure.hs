-- | Pure counterpart for binary search.

module Numeric.Search.Combinator.Pure
       (
         -- * Evidence
         M.Evidence(..),
         -- * Search Range
         M.Range,
         M.InitializesSearch,
         -- * Splitters
         M.splitForever, M.splitTill,

         -- * Search
         search,
         -- * Postprocess
         M.smallest, M.largest
       )where

import           Data.Functor.Identity
import qualified Numeric.Search.Combinator.Monadic as M


-- | Perform search over pure predicates. The monadic version of this is 'M.searchM' .
search :: (M.InitializesSearch a init, Eq b) =>
           init -> M.Splitter a -> (a -> b) -> [M.Range b a]
search init0 split0 pred0 = runIdentity $ M.searchM init0 split0 (Identity . pred0)
