module Zypr.Metadata where

import Prelude

type VarMetadata
  = {}

defaultVarMetadata :: VarMetadata
defaultVarMetadata = {}

type LamMetadata
  = { indent_bod :: Boolean
    }

defaultLamMetadata :: LamMetadata
defaultLamMetadata =
  { indent_bod: false
  }

type AppMetadata
  = { indent_arg :: Boolean
    }

defaultAppMetadata :: AppMetadata
defaultAppMetadata =
  { indent_arg: false
  }

type LetMetadata
  = { indent_imp :: Boolean
    , indent_bod :: Boolean
    }

defaultLetMetadata :: LetMetadata
defaultLetMetadata =
  { indent_imp: false
  , indent_bod: true
  }
