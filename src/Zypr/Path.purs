module Zypr.Path where

import Prelude
import Zypr.Syntax (Node, Term)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Path
  = Top
  | Zip
    { node :: Node -- Node of Term
    , lefts :: Array Term -- Terms to the left, reversed
    , up :: Path -- Path up
    , rights :: Array Term -- Terms to the right
    }

-- instances
derive instance genericPath :: Generic Path _

derive instance eqPath :: Eq Path

instance showPath :: Show Path where
  show x = genericShow x
