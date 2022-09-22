module Zypr.SyntaxTheme where

import Prelude
import Zypr.Metadata
import Zypr.Syntax
import Data.Array (concat)
import Data.String.CodeUnits as String
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props

type Res
  = Array ReactElement

type SyntaxTheme
  = { meta ::
        { name :: String
        }
    , id :: Id -> Res
    , term ::
        { var :: { id :: Res, md :: VarMetadata } -> Res
        , lam :: { var :: Res, bod :: Res, md :: LamMetadata, bod_assoc :: Boolean } -> Res
        , app :: { apl :: Res, arg :: Res, md :: AppMetadata, arg_assoc :: Boolean } -> Res
        , let_ :: { var :: Res, imp :: Res, bod :: Res, md :: LetMetadata } -> Res
        }
    }

basicSyntaxTheme :: SyntaxTheme
basicSyntaxTheme =
  { meta:
      { name: "basic"
      }
  , id:
      \id ->
        [ DOM.div [ Props.className "id" ]
            [ DOM.text (String.singleton id) ]
        ]
  , term:
      { var:
          \{ id, md } ->
            [ DOM.div [ Props.className "var" ]
                id
            ]
      , lam:
          \{ var, bod, md, bod_assoc } ->
            [ DOM.div [ Props.className "lam" ]
                (concat [ tk_lambda, tk_space, var, tk_space, tk_mapsto, tk_space, assocIf bod_assoc bod ])
            ]
      , app:
          \{ apl, arg, md } ->
            [ DOM.div [ Props.className "app" ]
                (concat [ apl, tk_space, arg ])
            ]
      , let_:
          \{ var, imp, bod, md } ->
            [ DOM.div [ Props.className "let" ]
                (concat [ tk_let, tk_space, var, tk_space, tk_assign, imp, tk_space, tk_in, tk_space, bod ])
            ]
      }
  }

-- Tokens
tk_space = makeStringToken " " "whitespace"

tk_lparen = makeStringToken "(" "punctuation paren lparen"

tk_rparen = makeStringToken ")" "punctuation paren rparen"

tk_lambda = makeStringToken "fun" "keyword"

tk_mapsto = makeStringToken "=>" "keyword"

tk_let = makeStringToken "let" "keyword"

tk_in = makeStringToken "in" "keyword"

tk_assign = makeStringToken ":=" "keyword"

assocIf b res = if b then concat [ tk_lparen, res, tk_rparen ] else res

makeStringToken :: String -> String -> Res
makeStringToken className str = makeToken className [ DOM.text str ]

makeToken :: String -> Res -> Res
makeToken className res = [ DOM.div [ Props.className ("token " <> className) ] res ]
