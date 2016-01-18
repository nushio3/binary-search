-- | Monadic binary search combinators.

{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, ScopedTypeVariables #-}

module Numeric.Search.Combinator.Monadic where

import           Control.Applicative((<$>))
import           Data.Sequence as Seq
import           Prelude hiding (init, pred)

-- * Evidence

-- | The 'Evidence' datatype is similar to 'Either' , but differes in that all 'CounterExample' values are
--   equal to each other, and all 'Example' values are also
--   equal to each other. The 'Evidence' type is used to binary-searching for some predicate and meanwhile returning evidences for that.

data Evidence a b = CounterExample a | Example b
                  deriving (Show, Read, Functor)

instance Eq (Evidence b a) where
  CounterExample _ == CounterExample _ = True
  Example _        == Example _        = True
  _                == _                = False

instance Ord (Evidence b a) where
  CounterExample _ `compare` CounterExample _ = EQ
  Example _        `compare` Example _        = EQ
  CounterExample _ `compare` Example _        = GT
  Example _        `compare` CounterExample _ = LT

instance Applicative (Evidence e) where
    pure                    = Example
    CounterExample  e <*> _ = CounterExample e
    Example f <*> r         = fmap f r

instance Monad (Evidence e) where
    return                  = Example
    CounterExample  l >>= _ = CounterExample l
    Example r >>= k         = k r

-- * Search range

-- | A type @x@ is an instance of 'SearchInitializer' @a@, if @x@ can be used to set up the lower and upper inital values for
-- binary search over values of type @a@.
class SearchInitializer a x where
  initializeSearchM :: Monad m => x -> (a -> m bool) -> m (a,a)

-- * Searching

-- | The generalized type for binary search functions.
type BinarySearchM m a b =
  InitializerM m a b ->
  CutterM m a b ->
  PredicateM m a b ->
  m (Seq (Range a b))

-- | 'BookEnd' comes in order [LEnd, REnd, LEnd, REnd ...], and
-- represents the ongoing state of the search results.
-- Two successive 'BookEnd' @LEnd x1 y1@, @REnd x2 y1@ represents a
-- claim that @pred x == y1@ for all @x@ such that @x1 <= x <= x2@ .
-- Like this:
--
-- > is (x^2 > 20000) ?
-- >
-- > LEnd    REnd  LEnd     REnd
-- > 0        100  200       300
-- > |_ False _|    |_ True  _|

data BookEnd a b
      = REnd !a !b
      | LEnd !a !b
      deriving (Eq, Show)

-- | 'Range' @((x1,x2),y)@ denotes that @pred x == y@ for all
-- @x1 <= x <= x2@ .
type Range a b = ((a,a),b)

-- | 'PredicateM' @m a b@ calculates the predicate in the context @m@.
type PredicateM m a b = a -> m b

-- | 'InitializerM' generates the initial set of ranges.
type InitializerM m a b = PredicateM m a b -> m (Seq (BookEnd a b))

-- | 'CutterM' @p x1 x2@ decides if we should further investigate the
-- gap between @x1@ and @x2@. If so, it gives a new value @x3@ wrapped
-- in a 'Just'. 'CutterM' may optionally use the predicate.
type CutterM m a b = PredicateM m a b -> a -> a -> m (Maybe a)


-- | an initializer with the initial range specified.
initConstM :: (Monad m) => a -> a -> InitializerM m a b
initConstM x1 x2 pred = do
  y1 <- pred x1
  y2 <- pred x2
  return $ Seq.fromList [LEnd x1 y1, REnd x1 y1,LEnd x2 y2, REnd x2 y2]

-- | an initializer that searches for the full bound.
initBoundedM :: (Monad m, Bounded a) => InitializerM m a b
initBoundedM = initConstM minBound maxBound

-- | a cutter for integral types.
cutIntegralM :: (Monad m, Integral a) => CutterM m a b
cutIntegralM _ x1 x2
  | x1+1 >= x2 = return Nothing
  | otherwise  = return $ Just ((x1+1)`div`2 + x2 `div`2)

-- | The most generalized version of search.
searchWithM :: forall m a b. (Functor m, Monad m, Eq b) => BinarySearchM m a b
searchWithM init cut pred = do
  seq0 <- init pred
  finalize <$> go seq0
  where
    go :: Seq (BookEnd a b) -> m (Seq (BookEnd a b))
    go seq0 = case viewl seq0 of
      EmptyL -> return seq0
      (x1 :< seq1) -> do
        let skip = (x1 <|) <$> go seq1
        case viewl seq1 of
          EmptyL -> skip
          (x2 :< seq2) -> case (x1,x2) of
            (REnd a1 b1, LEnd a2 b2) -> case b1==b2 of
              True  -> go seq2 -- merge the two regions
              False ->  do
                y1 <- drillDown a1 b1 a2 b2
                y2 <- go seq2
                return $ y1 >< y2
            _ -> skip

    -- precondition : b1 /= b2
    drillDown :: a -> b -> a -> b -> m (Seq (BookEnd a b))
    drillDown x1 y1 x2 y2 = do
      mc <- cut pred x1 x2
      case mc of
        Nothing -> return $ Seq.fromList [REnd x1 y1, LEnd x2 y2]
        Just x3 -> do
          y3 <- pred x3
          case () of
            _ | y3==y1 -> drillDown x3 y3 x2 y2
            _ | y3==y2 -> drillDown x1 y1 x3 y3
            _ -> do
              y1 <-  drillDown x1 y1 x3 y3
              y2 <-  drillDown x3 y3 x2 y2
              return $ y1 >< y2

    finalize :: Seq (BookEnd a b) -> Seq (Range a b)
    finalize seqE = case viewl seqE of
      EmptyL -> Seq.empty
      (x1 :< seqE1) -> case viewl seqE1 of
        EmptyL -> finalize seqE1
        (x2 :< seqE2) -> case (x1,x2) of
          (LEnd x1 y1, REnd x2 y2) | y1==y2 -> ((x1,x2), y1) <| finalize seqE2
          _                                 -> finalize seqE1
