module Zypr.Path where

import Prelude
import Zypr.Metadata
import Zypr.Syntax
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Path
  = Top
  | Lam_var { var :: Path, bod :: Term, md :: LamMetadata }
  | Lam_bod { var :: Term, bod :: Path, md :: LamMetadata }
  | App_apl { apl :: Path, arg :: Term, md :: AppMetadata }
  | App_arg { apl :: Term, arg :: Path, md :: AppMetadata }
  | Let_var { var :: Path, imp :: Term, bod :: Term, md :: LetMetadata }
  | Let_imp { var :: Term, imp :: Path, bod :: Term, md :: LetMetadata }
  | Let_bod { var :: Term, imp :: Term, bod :: Path, md :: LetMetadata }

derive instance genericPath :: Generic Path _

instance showPath :: Show Path where
  -- show x = genericShow x
  show = case _ of
    Top -> "@"
    Lam_var lam -> "(fun " <> show lam.var <> " => " <> show lam.bod <> ")"
    Lam_bod lam -> "(fun " <> show lam.var <> " => " <> show lam.bod <> ")"
    App_apl app -> "(" <> show app.apl <> " " <> show app.arg <> ")"
    App_arg app -> "(" <> show app.apl <> " " <> show app.arg <> ")"
    Let_var let_ -> "(let " <> show let_.var <> " = " <> show let_.imp <> " in " <> show let_.bod <> ")"
    Let_imp let_ -> "(let " <> show let_.var <> " = " <> show let_.imp <> " in " <> show let_.bod <> ")"
    Let_bod let_ -> "(let " <> show let_.var <> " = " <> show let_.imp <> " in " <> show let_.bod <> ")"
