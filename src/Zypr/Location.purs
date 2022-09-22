module Zypr.Location where

import Prelude
import Zypr.Metadata
import Zypr.Path
import Zypr.Syntax
import Data.Maybe (Maybe(..))

type Location
  = { term :: Term -- the Term at this Location
    , path :: Path -- the Path to the Term at this Location
    }

stepLeft :: Location -> Maybe Location
stepLeft loc = case loc.path of
  Top -> Nothing
  Lam_var lam -> Nothing -- TODO
  Lam_bod lam -> pure $ { term: lam.var, path: Lam_var { var: lam.bod, bod: loc.term, md: lam.md } }
  App_apl app -> Nothing -- TODO
  App_arg app -> pure $ { term: app.apl, path: App_apl { apl: app.arg, arg: loc.term, md: app.md } }
  Let_var let_ -> Nothing -- TODO
  Let_imp let_ -> pure $ { term: let_.var, path: Let_var { var: let_.imp, imp: loc.term, bod: let_.bod, md: let_.md } }
  Let_bod let_ -> pure $ { term: let_.imp, path: Let_imp { var: let_.var, imp: let_.bod, bod: loc.term, md: let_.md } }
