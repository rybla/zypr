module App
  ( AppProps, AppState, AppGiven, appClass, appComponent
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getState)
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.EditorTypes (EditorMode(..))
import Zypr.Example.Lambdas as Lambdas
import Zypr.Example.YCombinator as YCombinator
import Zypr.RenderEditor (editorClass)
import Zypr.SyntaxTheme (basicSyntaxTheme)

type AppProps
  = {}

type AppState
  = {}

type AppGiven
  = { state :: AppState, render :: Effect ReactElement }

appClass :: ReactClass AppProps
appClass = component "app" appComponent

appComponent :: ReactThis AppProps AppState -> Effect AppGiven
appComponent this =
  pure
    { state, render: render <$> getState this
    }
  where
  state :: AppState
  state = {}

  render :: AppState -> ReactElement
  render _ =
    DOM.div
      [ Props.className "app" ]
      [ createLeafElement editorClass
          { stateInit:
              { mode: TopMode { term: Lambdas.term }
              , history: []
              , syntaxTheme: basicSyntaxTheme
              , clipboard: Nothing
              , console: []
              , consoleVisible: false
              , introVisible: false
              , helpVisible: false
              }
          }
      ]

-- [ DOM.text "hello world"
-- , createLeafElement dropdownClass
--     { label: "Dropdown A"
--     , items: [ "a", "b", "c" ]
--     }
-- ]
{-
type DropdownProps
  = { label :: String, items :: Array String }

type DropdownState
  = { open :: Boolean, items :: Array String }

type DropdownGiven
  = { state :: DropdownState, render :: Effect ReactElement }

dropdownClass :: ReactClass DropdownProps
dropdownClass = component "dropdown" dropdownComponent

dropdownComponent :: ReactThis DropdownProps DropdownState -> Effect DropdownGiven
dropdownComponent this = do
  props <- getProps this
  let
    stateInit =
      { open: false
      , items: props.items
      }
  let
    render state =
      DOM.div [ Props.className "dropdown" ]
        [ DOM.div
            [ Props.className "dropdown-label"
            , Props.onClick \_event -> do
                Console.log "clicked on dropdown label"
                modifyState this \state' ->
                  state' { open = not state.open }
            ]
            [ DOM.text props.label ]
        , DOM.div
            ( [ Props.className
                  ( "dropdown-items"
                      <> if not state.open then " hidden" else ""
                  )
              ]
            )
            ( map
                ( \item ->
                    DOM.div [ Props.className "dropdown-item" ]
                      [ DOM.text item ]
                )
                state.items
            )
        ]
  pure
    { render: render <$> getState this
    , state: stateInit
    }
-}
