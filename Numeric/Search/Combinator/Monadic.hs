-- | Monadic binary search combinators.

{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, MultiWayIf, ScopedTypeVariables, TupleSections #-}

module Numeric.Search.Combinator.Monadic where

import           Control.Applicative((<$>))
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

-- | @(value, (lo,hi))@ represents the finding that @pred x == value@ for @lo <= x <= hi@.
-- By using this type, we can readily 'lookup' a list of 'Range' .

type Range b a = (b, (a,a))


-- | A type @x@ is an instance of 'SearchInitializer' @a@, if @x@ can be used to set up the lower and upper inital values for
-- binary search over values of type @a@.
-- .
-- 'initializeSearchM' should generate a list of 'Range' s, where each 'Range' has different -- predicate.
class InitializesSearch a x where
  initializeSearchM :: (Monad m, Eq b)=> x -> (a -> m b) -> m [Range b a]

-- | Set the lower and upper boundary explicitly.
instance InitializesSearch a (a,a) where
  initializeSearchM (lo,hi) pred0 = do
    pLo <- pred0 lo
    pHi <- pred0 hi
    return $ if | pLo == pHi -> [(,) pLo (lo,hi)]
                | otherwise  -> [(,) pLo (lo,lo), (,) pHi (hi,hi)]

-- | Set the lower and upper boundary from those first available from the lists.
instance InitializesSearch a ([a],[a]) where
  initializeSearchM ([], []) _ = return []
  initializeSearchM ([], x:_) pred0 = do
    p <- pred0 x
    return [(,) p (x,x)]
  initializeSearchM (x:_, []) pred0 = do
    p <- pred0 x
    return [(,) p (x,x)]
  initializeSearchM (lo:los,hi:his) pred0 = do
    pLo <- pred0 lo
    pHi <- pred0 hi
    let
      pop (p,x, []) = return (p,x,[])
      pop (p,_, x2:xs) = do
        p2 <- pred0 x2
        return (p2, x2, xs)

      go pez1@(p1,x1,xs1) pez2@(p2,x2,xs2)
          | p1 /= p2             = return [(,)p1 (x1,x1), (,)p2 (x2,x2)]
          | null xs1 && null xs2 = return [(,)p1 (x1,x2)]
          | otherwise = do
              pez1' <- pop pez1
              pez2' <- pop pez2
              go pez1' pez2'

    go (pLo, lo,los) (pHi, hi, his)

-- * Splitters

type Splitter a = a -> a -> Maybe a

-- | Perform split forever, until we cannot find a mid-value due to machine precision.
splitForever :: Integral a => Splitter a
splitForever lo hi = let mid = lo `div` 2 + hi `div` 2 in
  if lo == mid || mid == hi then Nothing
  else Just mid

-- | Perform splitting until @hi - lo <= eps@ .
splitTill :: Integral a => a -> Splitter a
splitTill eps lo hi
  | hi - lo <= eps = Nothing
  | otherwise      = splitForever lo hi

-- * Searching

-- | Mother of all search variations.
--
-- 'searchM' carefully keeps track of the latest predicate found, so that it works well with the 'Evidence' class.

searchM :: forall a m b init . (Monad m, InitializesSearch a init, Eq b) =>
           init -> Splitter a -> (a -> m b) -> m [Range b a]
searchM init0 split0 pred0 = do
  ranges0 <- initializeSearchM init0 pred0
  go ranges0
    where
      go :: [Range b a] -> m [Range b a]
      go (r1@(p1, (lo1, hi1)):r2@(p2, (lo2, hi2)):rest) = case split0 hi1 lo2 of
        Nothing   -> (r1:) <$> go (r2:rest)
        Just mid1 -> do
          pMid <- pred0 mid1
          if | p1 == pMid -> go $ (pMid, (lo1,mid1)) : r2 : rest
             | p2 == pMid -> go $ r1 : (pMid, (mid1,hi2)) : rest
             | otherwise  -> go $ r1 : (pMid, (mid1,mid1)) : r2 : rest
      go xs = return xs

-- * Postprocess

-- | Pick up the smallest @a@ that satisfies @pred a == b@ .
smallest :: (Eq b) => b -> [Range b a] -> Maybe a
smallest b rs = fst <$> lookup b rs


-- | Pick up the largest @a@ that satisfies @pred a == b@ .
largest :: (Eq b) => b -> [Range b a] -> Maybe a
largest b rs = snd <$>lookup b rs
