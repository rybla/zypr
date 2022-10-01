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
        { var :: { dat :: VarData, id :: Res } -> Res
        , lam :: { dat :: LamData, bnd :: Res, bod :: Res, isAss :: Boolean, bod_isLam :: Boolean, isLamBod :: Boolean } -> Res
        , app :: { dat :: AppData, apl :: Res, arg :: Res, apl_isApp :: Boolean, isApl :: Boolean, isAss :: Boolean } -> Res
        , let_ :: { dat :: LetData, bnd :: Res, imp :: Res, bod :: Res, isAss :: Boolean } -> Res
        , hole :: { dat :: HoleData } -> Res
        , plus :: { dat :: PlusData, left :: Res, right :: Res, isAss :: Boolean } -> Res
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
          \{ dat, id } -> id
      , lam:
          \{ dat, bnd, bod, isAss, isLamBod, bod_isLam } ->
            assocIf isAss $ concat
              $ if isLamBod && bod_isLam then
                  [ bnd, tk_lamArgHandle, bod ]
                else if not isLamBod && bod_isLam then
                  [ tk_lambda, tk_space, bnd, tk_lamArgHandle, bod ]
                else if isLamBod && not bod_isLam then
                  [ bnd, tk_space, tk_mapsto, tk_space, bod ]
                else
                  [ tk_lambda, tk_space, bnd, tk_space, tk_mapsto, tk_space, bod ]
      , app:
          \{ dat, apl, arg, apl_isApp, isApl, isAss } ->
            -- sep = if apl_isApp then [] else [ tk_space ]
            -- wrap = if isApl then assocIf isAss <<< (tk_aplHandle <> _) <<< (_ <> tk_appHandle) else assocIf isAss <<< (tk_aplHandle <> _)
            -- wrap = assocIf isAss <<< (_ <> tk_appHandle)
            -- assocIf isAss $ concat $ [ apl, tk_appHandle, arg ]
            assocIf isAss
              if isApl && apl_isApp then
                concat [ apl, arg, tk_appHandle ]
              else if isApl && not apl_isApp then
                concat [ apl, tk_space, arg, tk_appHandle ]
              else
                concat [ apl, tk_space, arg ]
      , let_:
          \{ dat, bnd, imp, bod, isAss } ->
            assocIf isAss $ concat [ tk_let, tk_space, bnd, tk_space, tk_assign, tk_space, imp, tk_space, tk_in, tk_space, bod ]
      , hole:
          \{ dat } -> tk_question
      , plus:
          \{ dat, left, right, isAss } ->
            assocIf isAss $ concat [ left, tk_space, tk_plus, tk_space, right ]
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

-- Tokens
tk_question :: Res
tk_question = makeStringToken "" "?"

tk_app :: Res
tk_app = makeStringToken "keyword" "app"

tk_lamArgHandle :: Res
tk_lamArgHandle = makeStringToken "punc punc-lamArgHandle" "▪"

tk_appHandle :: Res
tk_appHandle = makeStringToken "punc punc-appHandle" "▪"

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

makeStringToken :: String -> String -> Res
makeStringToken className str = makeToken className [ DOM.text str ]

makeToken :: String -> Res -> Res
makeToken className res = [ DOM.div [ Props.className ("token " <> className) ] res ]
