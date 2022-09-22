module Zypr.RenderEditor where

import Prelude
import Effect (Effect)
import React (ReactClass, ReactElement, ReactThis, component, getProps, getState)
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.RenderSyntax (renderLocation)
import Zypr.EditorTypes (EditorGiven, EditorProps, EditorState)

editorClass :: ReactClass EditorProps
editorClass = component "editor" editorComponent

editorComponent :: ReactThis EditorProps EditorState -> Effect EditorGiven
editorComponent this = do
  props <- getProps this
  pure
    { state: props.stateInit
    , render: render <$> getState this
    }
  where
  render :: EditorState -> ReactElement
  render state =
    DOM.div
      [ Props.className "editor" ]
      (renderLocation state.syntaxTheme state.location)
