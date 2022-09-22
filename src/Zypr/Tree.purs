module Zypr.Tree where

import Prelude

data Tree a
  = Leaf a
  | Node (Array (Tree a))
