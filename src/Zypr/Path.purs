module Zypr.Path where

import Prelude
import Zypr.Syntax
import Zypr.Metadata

data Path
  = Top
  | Lam_var { var :: Path, bod :: Term, md :: LamMetadata }
  | Lam_bod { var :: Term, bod :: Path, md :: LamMetadata }
  | App_apl { apl :: Path, arg :: Term, md :: AppMetadata }
  | App_arg { apl :: Term, arg :: Path, md :: AppMetadata }
  | Let_var { var :: Path, imp :: Term, bod :: Term, md :: LetMetadata }
  | Let_imp { var :: Term, imp :: Path, bod :: Term, md :: LetMetadata }
  | Let_bod { var :: Term, imp :: Term, bod :: Path, md :: LetMetadata }
