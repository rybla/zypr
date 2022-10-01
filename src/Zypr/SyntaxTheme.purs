module Zypr.SyntaxTheme where

import Prelude
import Data.Array (concat)
import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.Syntax

type Res
  = Array ReactElement

type SyntaxTheme
  = { meta ::
        { name :: String
        }
    , term ::
        { var :: { dat :: VarData, id :: Res, isApl :: Boolean } -> Res
        , lam :: { dat :: LamData, bnd :: Res, bod :: Res, isAss :: Boolean, bod_isLam :: Boolean, isLamBod :: Boolean, isApl :: Boolean } -> Res
        , app :: { dat :: AppData, apl :: Res, arg :: Res, apl_isApp :: Boolean, isApl :: Boolean, isAss :: Boolean } -> Res
        , let_ :: { dat :: LetData, bnd :: Res, imp :: Res, bod :: Res, isAss :: Boolean, isApl :: Boolean } -> Res
        , hole :: { dat :: HoleData, isApl :: Boolean } -> Res
        , plus :: { dat :: PlusData, left :: Res, right :: Res, isAss :: Boolean, isApl :: Boolean } -> Res
        }
    }

syntaxThemes :: Array SyntaxTheme
syntaxThemes =
  [ basicSyntaxTheme
  , appSyntaxTheme
  , emojiSyntaxTheme
  , judsonSyntaxTheme
  ]

basicSyntaxTheme :: SyntaxTheme
basicSyntaxTheme =
  { meta:
      { name: "basic"
      }
  , term:
      { var:
          \{ dat, id, isApl } -> appHandleIf isApl id
      , lam:
          \{ dat, bnd, bod, isAss, isLamBod, bod_isLam, isApl } ->
            appHandleIf isApl <<< assocIf isAss
              $ concat
                  if isLamBod && bod_isLam then
                    [ tk_lamArgHandle, bnd, bod ]
                  else if not isLamBod && bod_isLam then
                    [ tk_lambda, tk_lamArgHandle, bnd, bod ]
                  else if isLamBod && not bod_isLam then
                    [ tk_lamArgHandle, bnd, tk_mapsto, bod ]
                  else
                    [ tk_lambda, bnd, tk_mapsto, bod ]
      , app:
          \{ dat, apl, arg, apl_isApp, isApl, isAss } ->
            -- sep = if apl_isApp then [] else [ tk_space ]
            -- wrap = if isApl then assocIf isAss <<< (tk_aplHandle <> _) <<< (_ <> tk_appHandle) else assocIf isAss <<< (tk_aplHandle <> _)
            -- wrap = assocIf isAss <<< (_ <> tk_appHandle)
            -- assocIf isAss $ concat $ [ apl, tk_appHandle, arg ]
            appHandleIf isApl <<< assocIf isAss
              $ concat [ apl, arg ]
      , let_:
          \{ dat, bnd, imp, bod, isAss, isApl } ->
            appHandleIf isApl <<< assocIf isAss
              $ concat [ tk_let, tk_space, bnd, tk_space, tk_assign, tk_space, imp, tk_space, tk_in, tk_space, bod ]
      , hole:
          \{ dat, isApl } ->
            appHandleIf isApl
              $ res_hole
      , plus:
          \{ dat, left, right, isAss, isApl } ->
            appHandleIf isApl <<< assocIf isAss
              $ concat [ left, tk_space, tk_plus, tk_space, right ]
      }
  }

emojiSyntaxTheme :: SyntaxTheme
emojiSyntaxTheme =
  basicSyntaxTheme
    { meta
      { name = "emoji" }
    , term
      { lam =
        \{ dat, bnd, bod } ->
          assoc $ concat [ tk_zipper, tk_space, bnd, tk_space, tk_zipper, tk_space, bod ]
      , app =
        \{ dat, apl, arg } ->
          assoc $ concat [ apl, tk_space, arg ]
      , let_ =
        \{ dat, bnd, imp, bod } ->
          assoc $ concat [ tk_zipper, tk_space, bnd, tk_space, tk_zipper, tk_space, imp, tk_space, tk_zipper, tk_space, bod ]
      }
    }

appSyntaxTheme :: SyntaxTheme
appSyntaxTheme =
  basicSyntaxTheme
    { meta
      { name = "app" }
    , term
      { app = \{ dat, apl, arg } -> assoc $ concat [ tk_app, tk_space, apl, tk_space, arg ] }
    }

judsonSyntaxTheme :: SyntaxTheme
judsonSyntaxTheme =
  basicSyntaxTheme
    { meta
      { name = "the judson"
      }
    , term
      { lam =
        \{ dat, bnd, bod } ->
          assoc $ concat [ gideon, tk_space, bnd, tk_space, sundback, tk_space, bod ]
      , app =
        \{ dat, apl, arg } ->
          assoc $ concat [ apl, tk_space, arg ]
      , let_ =
        \{ dat, bnd, imp, bod } ->
          assoc $ concat [ whitcomb, tk_space, bnd, tk_space, l, tk_space, imp, tk_space, judson, tk_space, bod ]
      }
    }
  where
  judson = makeStringToken "keyword" "Judson"

  l = makeStringToken "keyword" "L."

  whitcomb = makeStringToken "keyword" "Whitcomb"

  gideon = makeStringToken "keyword" "Gideon"

  sundback = makeStringToken "keyword" "Sundback"

-- React Elements
res_hole :: Res
res_hole = [ DOM.div [ Props.className "hole" ] tk_question ]

-- Tokens
tk_question :: Res
tk_question = makeStringToken "" "?"

tk_app :: Res
tk_app = makeStringToken "keyword" "app"

tk_lamArgHandle :: Res
tk_lamArgHandle = makeStringToken "punc punc-lamArgHandle" "●"

tk_appHandle :: Res
tk_appHandle = makeStringToken "punc punc-appHandle" "●"

tk_aplHandle :: Res
tk_aplHandle = makeStringToken "punc punc-aplHandle" "$"

tk_space :: Res
tk_space = makeStringToken "space" " "

tk_lparen :: Res
tk_lparen = makeStringToken "punc punc-paren punc-lparen" "("

tk_rparen :: Res
tk_rparen = makeStringToken "punc punc-paren punc-rparen rparen" ")"

tk_lambda :: Res
tk_lambda = makeStringToken "keyword keyword-lambda" "λ"

tk_mapsto :: Res
-- tk_mapsto = makeStringToken "keyword keyword-mapsto" "⇒"
tk_mapsto = makeStringToken "keyword keyword-mapsto" "↦"

tk_let :: Res
tk_let = makeStringToken "keyword keyword-let" "let"

tk_in :: Res
tk_in = makeStringToken "keyword keyword-in" "in"

tk_assign :: Res
tk_assign = makeStringToken "keyword" "="

tk_zipper :: Res
tk_zipper = makeStringToken "keyword emoji" "🖇️"

tk_plus :: Res
tk_plus = makeStringToken "keyword" "+"

assoc :: Res -> Res
assoc res = concat [ tk_lparen, res, tk_rparen ]

assocIf :: Boolean -> Res -> Res
assocIf b res = if b then assoc res else res

appHandleIf :: Boolean -> Res -> Res
appHandleIf b res = if b then res <> tk_appHandle else res

makeStringToken :: String -> String -> Res
makeStringToken className str = makeToken className [ DOM.text str ]

makeToken :: String -> Res -> Res
makeToken className res = [ DOM.div [ Props.className ("token " <> className) ] res ]
