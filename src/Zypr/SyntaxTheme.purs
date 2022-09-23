module Zypr.SyntaxTheme where

import Prelude
import Data.Array (concat)
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.Metadata
import Zypr.Syntax

type Res
  = Array ReactElement

type SyntaxTheme
  = { meta ::
        { name :: String
        }
    , term ::
        { var :: { var :: Var, id :: Res } -> Res
        , lam :: { lam :: Lam, bnd :: Res, bod :: Res } -> Res
        , app :: { app :: App, apl :: Res, arg :: Res } -> Res
        , let_ :: { let_ :: Let, bnd :: Res, imp :: Res, bod :: Res } -> Res
        }
    }

basicSyntaxTheme :: SyntaxTheme
basicSyntaxTheme =
  { meta:
      { name: "basic"
      }
  , term:
      { var:
          \{ var, id } -> id
      , lam:
          \{ lam, bnd, bod } ->
            concat [ tk_lambda, tk_space, bnd, tk_space, tk_mapsto, tk_space, assoc bod ]
      , app:
          \{ app, apl, arg } ->
            concat [ apl, tk_space, arg ]
      , let_:
          \{ let_, bnd, imp, bod } ->
            concat [ tk_let, tk_space, bnd, tk_space, tk_assign, imp, tk_space, tk_in, tk_space, bod ]
      }
  }

-- Tokens
tk_space :: Res
tk_space = makeStringToken "space" " "

tk_lparen :: Res
tk_lparen = makeStringToken "punc punc-paren punc-lparen" "("

tk_rparen :: Res
tk_rparen = makeStringToken "punc punc-paren punc-rparen rparen" ")"

tk_lambda :: Res
tk_lambda = makeStringToken "keyword keyword-fun" "fun"

tk_mapsto :: Res
tk_mapsto = makeStringToken "keyword keyword-mapsto" "=>"

tk_let :: Res
tk_let = makeStringToken "keyword keyword-let" "let"

tk_in :: Res
tk_in = makeStringToken "keyword keyword-in" "in"

tk_assign :: Res
tk_assign = makeStringToken "keyword" ":="

assoc :: Res -> Res
assoc res = concat [ tk_lparen, res, tk_rparen ]

assocIf :: Boolean -> Res -> Res
assocIf b res = if b then assoc res else res

makeStringToken :: String -> String -> Res
makeStringToken className str = makeToken className [ DOM.text str ]

makeToken :: String -> Res -> Res
makeToken className res = [ DOM.div [ Props.className ("token " <> className) ] res ]
