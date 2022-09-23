module Zypr.Location where

import Data.Tuple.Nested
import Prelude
import Zypr.Metadata
import Zypr.Path
import Zypr.Syntax
import Data.Array (null, reverse, uncons, unsnoc, (:))
import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe (unsafeThrow)

type Location
  = { term :: Term -- the Term at this Location
    , path :: Path -- the Path to the Term at this Location
    }

stepRight :: Location -> Maybe Location
stepRight loc = case loc.path of
  Zip { dat, lefts, up, rights }
    | Just { head: term, tail: rights' } <- uncons rights ->
      pure
        { term
        , path: Zip { dat, lefts: loc.term : lefts, up, rights: rights' }
        }
  _ -> Nothing

stepNext :: Location -> Maybe Location
stepNext loc = case stepRight loc of
  Just loc' -> pure loc'
  Nothing -> stepDown loc

stepLeft :: Location -> Maybe Location
stepLeft loc = case loc.path of
  Zip { dat, lefts, up, rights }
    | Just { head: term, tail: lefts' } <- uncons lefts ->
      pure
        { term
        , path: Zip { dat, lefts: lefts', up, rights: loc.term : rights }
        }
  _ -> Nothing

stepPrev :: Location -> Maybe Location
stepPrev loc = case stepLeft loc of
  Just loc' -> pure loc'
  Nothing -> stepUp loc

stepDown :: Location -> Maybe Location
stepDown loc = case toGenTerm loc.term of
  { dat, terms }
    | Just { head: term, tail: terms' } <- uncons terms ->
      pure
        { term: term
        , path: Zip { dat, lefts: [], up: loc.path, rights: terms' }
        }
  _ -> Nothing

-- | Example: pickN [1,2,3,4,5] 2 = Just { lefts: [2,1], pick: 3, rights: [4,5] }
pickN ::
  forall a.
  Array a ->
  Int ->
  Maybe { lefts :: Array a, pick :: a, rights :: Array a }
pickN = go []
  where
  go ::
    Array a ->
    Array a ->
    Int ->
    Maybe { lefts :: Array a, pick :: a, rights :: Array a }
  go lefts arr n
    | n == 0, Just { head: pick, tail: rights } <- uncons arr = Just { lefts, pick, rights }
    | n > 0, Just { head: left, tail: arr' } <- uncons arr = go (left : lefts) arr' (n - 1)
    | otherwise = Nothing

-- steps down to the Nth child of Term
stepDownN :: Location -> Int -> Maybe Location
stepDownN loc n = case toGenTerm loc.term of
  { dat, terms }
    | Just { lefts, pick: term, rights } <- pickN terms n ->
      pure
        { term
        , path: Zip { dat, lefts, up: loc.path, rights }
        }
  _ -> Nothing

stepUp :: Location -> Maybe Location
stepUp loc = case loc.path of
  Zip { dat, lefts, up, rights } ->
    pure
      { term: fromGenTerm { dat, terms: reverse lefts <> [ loc.term ] <> rights }
      , path: up
      }
  _ -> Nothing

siblings :: Location -> { lefts :: Array Location, rights :: Array Location }
siblings loc = { lefts: goLeft [] loc, rights: goRight [] loc }
  where
  goLeft lefts loc = case stepLeft loc of
    Nothing -> lefts
    Just loc' -> goLeft (loc' : lefts) loc'

  goRight rights loc = case stepRight loc of
    Nothing -> rights
    Just loc' -> goRight (loc' : rights) loc'

-- step down, then step right until can't
children :: Location -> Array Location
children loc = case stepDown loc of
  Just loc' -> go [] loc'
  Nothing -> []
  where
  go locs loc = case stepRight loc of
    Just loc' -> go (loc : locs) loc'
    Nothing -> reverse (loc : locs)
