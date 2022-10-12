module Zypr.Location where

import Data.Tuple.Nested
import Prelude
import Zypr.Path
import Zypr.Syntax

import Data.Array (null, reverse, uncons, unsnoc, (:))
import Data.Maybe (Maybe(..), maybe)
import Effect.Exception.Unsafe (unsafeThrow)
import Text.PP as PP
import Undefined (undefined)

type Location
  = { syn :: Syntax -- the Syntax at this Location
    , path :: Path -- the Path to the Syntax at this Location
    }

-- ppLocation
ppLocation :: Location -> PP.Doc
ppLocation loc =
  PP.words
    [ PP.pp "{"
    , PP.pp "syn:"
    , PP.pp loc.syn
    , PP.pp ","
    , PP.pp "path:"
    , PP.pp loc.path
    , PP.pp "}"
    ]

-- step
stepRight :: Location -> Maybe Location
stepRight loc = case loc.path of
  Zip { dat, lefts, up, rights }
    | Just { head: syn, tail: rights' } <- uncons rights ->
      pure
        { syn
        , path: Zip { dat, lefts: loc.syn : lefts, up, rights: rights' }
        }
  _ -> Nothing

stepNext :: Location -> Maybe Location
stepNext loc = case stepDown loc of
  Just loc' -> pure loc'
  Nothing ->
    let
      go loc = case stepRight loc of
        Just loc' -> pure loc'
        Nothing -> go =<< stepUp loc
    in
      go loc

stepLeft :: Location -> Maybe Location
stepLeft loc = case loc.path of
  Zip { dat, lefts, up, rights }
    | Just { head: syn, tail: lefts' } <- uncons lefts ->
      pure
        { syn
        , path: Zip { dat, lefts: lefts', up, rights: loc.syn : rights }
        }
  _ -> Nothing

stepPrev :: Location -> Maybe Location
stepPrev loc = case stepLeft loc of
  Just loc' -> case stepRightmostDescendant loc' of
    Just loc'' -> pure loc''
    Nothing -> pure loc'
  Nothing -> stepUp loc

stepDown :: Location -> Maybe Location
stepDown loc = case toGenSyntax loc.syn of
  { dat, syns }
    | Just { head: syn, tail: syns' } <- uncons syns ->
      pure
        { syn
        , path: Zip { dat, lefts: [], up: loc.path, rights: syns' }
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
stepDownN loc n = case toGenSyntax loc.syn of
  { dat, syns }
    | Just { lefts, pick: syn, rights } <- pickN syns n ->
      pure
        { syn
        , path: Zip { dat, lefts, up: loc.path, rights }
        }
  _ -> Nothing

stepUp :: Location -> Maybe Location
stepUp loc = case loc.path of
  Zip { dat, lefts, up, rights } ->
    pure
      { syn: fromGenSyntax { dat, syns: reverse lefts <> [ loc.syn ] <> rights }
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
    Nothing -> reverse rights
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

-- step to this Location's leftmost descendant 
stepLeftmostDescendant :: Location -> Maybe Location
stepLeftmostDescendant loc = do
  loc' <- stepDown loc
  let
    go loc = maybe (pure loc) go (stepDown loc)
  go loc'

-- step to this Location's rightmost descendant
stepRightmostDescendant :: Location -> Maybe Location
stepRightmostDescendant loc = do
  loc' <- stepDown loc
  let
    goRight loc = maybe (goDown loc) goRight (stepRight loc)

    goDown loc = maybe (pure loc) goRight (stepDown loc)
  goRight loc'

stepNextHole :: Location -> Maybe Location
stepNextHole loc = do
  loc' <- stepNext loc
  case loc'.syn of
    TermSyntax (Hole _) -> pure loc'
    _ -> stepNextHole loc'

stepPrevHole :: Location -> Maybe Location
stepPrevHole loc = do
  loc' <- stepPrev loc
  case loc'.syn of
    TermSyntax (Hole _) -> pure loc'
    _ -> stepPrevHole loc'

wrapPath :: Path -> Syntax -> Syntax
wrapPath Top syn = syn
wrapPath (Zip {dat, lefts, up, rights}) syn = wrapPath up (wrapOne {dat , lefts, rights} syn)

wrapOne :: { dat :: SyntaxData , lefts :: Array Syntax , rights :: Array Syntax }
  -> Syntax -> Syntax
wrapOne {dat, lefts, rights} syn
  = fromGenSyntax
    { dat: dat
      , syns: lefts <> [syn] <> rights
      }