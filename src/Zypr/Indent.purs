module Zypr.Indent where

import Prelude

import Data.Maybe (Maybe(..))
import Zypr.Syntax (SyntaxData(..), TermData(..))

toggleIndentData :: SyntaxData -> Int -> Maybe SyntaxData
toggleIndentData (TermData (LamData {indent_bod})) 1 = Just $ TermData $ LamData {indent_bod: not indent_bod}
toggleIndentData (TermData (AppData {indent_arg})) 1 = Just $ TermData $ AppData {indent_arg : not indent_arg}
toggleIndentData (TermData (LetData dat)) 1
    = Just $ TermData $ (LetData (dat {indent_imp = not dat.indent_imp}))
toggleIndentData (TermData (LetData dat)) 2
    = Just $ TermData $ (LetData (dat {indent_bod = not dat.indent_bod}))
toggleIndentData _ i = Nothing