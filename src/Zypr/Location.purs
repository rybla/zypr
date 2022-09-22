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
  Lam_var lam -> stepUp loc
  Lam_bod lam -> pure { term: lam.var, path: Lam_var { var: lam.bod, bod: loc.term, md: lam.md } }
  -- App_apl app -> Nothing -- TODO
  -- App_arg app -> pure { term: app.apl, path: App_apl { apl: app.arg, arg: loc.term, md: app.md } }
  -- Let_var let_ -> Nothing -- TODO
  -- Let_imp let_ -> pure { term: let_.var, path: Let_var { var: let_.imp, imp: loc.term, bod: let_.bod, md: let_.md } }
  -- Let_bod let_ -> pure { term: let_.imp, path: Let_imp { var: let_.var, imp: let_.bod, bod: loc.term, md: let_.md } }
  _ -> Nothing

stepRight :: Location -> Maybe Location
stepRight loc = case loc.path of
  Top -> stepDown loc
  Lam_var lam -> pure { term: lam.bod, path: Lam_bod { var: loc.term, bod: lam.var, md: lam.md } }
  Lam_bod lam -> stepDown loc
  -- App_apl app -> pure { term: app.arg, path: App_arg { apl: loc.term, arg: app.apl, md: app.md } }
  -- App_arg app -> Nothing
  -- Let_var let_ -> pure { term: let_.imp, path: Let_imp { var: loc.term, imp: let_.var, bod: let_.bod, md: let_.md } }
  -- Let_imp let_ -> pure { term: let_.bod, path: Let_bod { var: let_.bod, imp: loc.term, bod: let_.imp, md: let_.md } }
  -- Let_bod let_ -> Nothing
  _ -> Nothing

stepDown :: Location -> Maybe Location
stepDown loc = case loc.term of
  Var var -> Nothing
  Lam lam -> pure { term: lam.var, path: Lam_var { var: loc.path, bod: lam.bod, md: lam.md } }
  -- App app -> ?a
  -- Let let_ -> ?a
  _ -> Nothing

-- stepDown loc = case loc.path of
--   Top -> Nothing
--   Lam_var lam -> Nothing
--   Lam_bod lam -> Nothing
--   App_apl app -> Nothing
--   App_arg app -> Nothing
--   Let_var let_ -> Nothing
--   Let_imp let_ -> Nothing
--   Let_bod let_ -> Nothing
stepUp :: Location -> Maybe Location
stepUp loc = case loc.path of
  Top -> Nothing
  Lam_var lam -> pure { term: Lam { var: loc.term, bod: lam.bod, md: lam.md }, path: lam.var }
  Lam_bod lam -> pure { term: Lam { var: lam.var, bod: loc.term, md: lam.md }, path: lam.bod }
  -- App_apl app -> Nothing
  -- App_arg app -> Nothing
  -- Let_var let_ -> Nothing
  -- Let_imp let_ -> Nothing
  -- Let_bod let_ -> Nothing
  _ -> Nothing
