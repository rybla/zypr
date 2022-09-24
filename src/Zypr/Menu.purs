module Zypr.Menu where

import Prelude
import Zypr.EditorTypes
import Data.Array (concat)
import Effect (Effect)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.EditorEffect (EditorEffect, runEditorEffect)
import Zypr.EditorEffect as EditorEffect
import Zypr.Example.Applications as Applications
import Zypr.Example.Lambdas as Lambdas
import Zypr.Example.YCombinator as YCombinator
import Zypr.SyntaxTheme (Res, basicSyntaxTheme, syntaxThemes)

renderMenu :: EditorThis -> EditorState -> Res
renderMenu this _state =
  [ DOM.div [ Props.className "menu" ]
      $ [ DOM.div [ Props.className "menu-title" ]
            -- [ DOM.text "(🖇️ zypr) " ]
            -- [ DOM.text "(⛓️ ▪ zypr) " ]
            -- [ DOM.text "(z y▪p▪r)"]
            -- [ DOM.text "[zypr]" ]
            [ DOM.text "|#|zypr|#|" ]
        , createLeafElement menuItemClass
            { thisEditor: this
            , title: "examples"
            , options:
                [ { label: "Lambdas.zypr"
                  , onClick: EditorEffect.setTerm Lambdas.term
                  }
                , { label: "Applications.zypr"
                  , onClick: EditorEffect.setTerm Applications.term
                  }
                , { label: "YCombinator.zypr"
                  , onClick: EditorEffect.setTerm YCombinator.term
                  }
                ]
            }
        , createLeafElement menuItemClass
            { thisEditor: this
            , title: "syntax"
            , options:
                map
                  ( \thm ->
                      { label: thm.meta.name
                      , onClick: EditorEffect.setSyntaxTheme thm
                      }
                  )
                  syntaxThemes
            }
        , DOM.a
            [ Props.className "menu-item"
            , Props.href "https://github.com/Riib11/zypr/"
            ]
            [ DOM.div [ Props.className "menu-item-title" ]
                [ DOM.text "github" ]
            ]
        ]
  ]

type MenuItemProps
  = { title :: String
    , options :: Array { label :: String, onClick :: EditorEffect Unit }
    , thisEditor :: EditorThis
    }

type MenuItemState
  = { open :: Boolean }

type MenuItemGiven
  = { state :: MenuItemState, render :: Effect ReactElement }

menuItemClass :: ReactClass MenuItemProps
menuItemClass = component "menu-item" menuItemComponent

menuItemComponent ::
  ReactThis MenuItemProps MenuItemState ->
  Effect MenuItemGiven
menuItemComponent this = do
  props <- getProps this
  let
    render state =
      DOM.div
        [ Props.className $ "menu-item "
            <> if state.open then "open" else "closed"
        ]
        $ [ DOM.div
              [ Props.className $ "menu-item-title"
              , Props.onClick \_event ->
                  modifyState this _ { open = not state.open }
              ]
              [ DOM.text props.title ]
          , DOM.div
              [ Props.className $ "menu-item-options " ]
              $ map
                  ( \opt ->
                      DOM.div
                        [ Props.className "menu-item-option"
                        , Props.onClick \event -> do
                            runEditorEffect props.thisEditor opt.onClick
                            modifyState this _ { open = false }
                        ]
                        [ DOM.text opt.label ]
                  )
                  props.options
          ]
  pure
    { state:
        { open: false
        }
    , render: render <$> getState this
    }
