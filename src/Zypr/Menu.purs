module Zypr.Menu where

import Prelude
import Zypr.EditorTypes
import Data.Array (concat)
import Effect (Effect)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (stopPropagation)
import Zypr.EditorEffect (EditorEffect, runEditorEffect, toggleHelpVisible, toggleIntroVisible)
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
            [ DOM.text "•[zypr]•" ]
        , createLeafElement menuItemDropdownClass
            { title: "examples"
            , thisEditor: this
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
        {- TODO: tmp disabled alt syntax themes
        , createLeafElement menuItemDropdownClass
            { title: "syntax"
            , thisEditor: this
            , options:
                map
                  ( \thm ->
                      { label: thm.meta.name
                      , onClick: EditorEffect.setSyntaxTheme thm
                      }
                  )
                  syntaxThemes
            }
        -}
        , createLeafElement menuItemDropdownClass
            { title: "docs"
            , thisEditor: this
            , options:
                [ { label: "intro"
                  , onClick: EditorEffect.toggleIntroVisible
                  }
                , { label: "help"
                  , onClick: EditorEffect.toggleHelpVisible
                  }
                ]
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

-- MenuItemDropdown 
type MenuItemDropdownProps
  = { title :: String
    , options :: Array { label :: String, onClick :: EditorEffect Unit }
    , thisEditor :: EditorThis
    }

type MenuItemDropdownState
  = { open :: Boolean }

type MenuItemDropdownGiven
  = { state :: MenuItemDropdownState, render :: Effect ReactElement }

menuItemDropdownClass :: ReactClass MenuItemDropdownProps
menuItemDropdownClass = component "menu-item" menuItemDropdownComponent

menuItemDropdownComponent ::
  ReactThis MenuItemDropdownProps MenuItemDropdownState ->
  Effect MenuItemDropdownGiven
menuItemDropdownComponent this = do
  props <- getProps this
  let
    render state =
      DOM.div
        [ Props.className $ "menu-item menu-item-dropdown "
            <> if state.open then "open" else "closed"
        ]
        [ DOM.div
            [ Props.className $ "menu-item-title"
            , Props.onClick \event -> do
                stopPropagation event
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
                          stopPropagation event
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

-- MenuItemButton 
type MenuItemButtonProps
  = { title :: String
    , thisEditor :: EditorThis
    , effect :: Effect Unit
    }

type MenuItemButtonState
  = {}

type MenuItemButtonGiven
  = { state :: MenuItemButtonState, render :: Effect ReactElement }

menuItemButtonClass :: ReactClass MenuItemButtonProps
menuItemButtonClass = component "menu-item" menuItemButtonComponent

menuItemButtonComponent ::
  ReactThis MenuItemButtonProps MenuItemButtonState ->
  Effect MenuItemButtonGiven
menuItemButtonComponent this = do
  props <- getProps this
  let
    render state =
      DOM.div
        [ Props.className $ "menu-item menu-item-dropdown" ]
        [ DOM.div
            [ Props.className $ "menu-item-title"
            , Props.onClick \event -> do
                stopPropagation event
                props.effect
            ]
            [ DOM.text props.title ]
        ]
  pure
    { state: {}
    , render: render <$> getState this
    }
