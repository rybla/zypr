module Zypr.EditorEffect where

import Data.Tuple.Nested
import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, get, modify_, put, runStateT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import React (ReactThis, getProps, getState, modifyState)
import Zypr.EditorConsole (logEditorConsole, stringEditorConsoleError)
import Zypr.EditorTypes (EditorState, EditorProps)
import Zypr.Location (Location)
import Zypr.Location as Location
import Zypr.SyntaxTheme (Res)

type EditorEffect a
  = StateT EditorState (ReaderT EditorProps (ExceptT String Effect)) a

runEditorEffect :: ReactThis EditorProps EditorState -> EditorEffect Unit -> Effect Unit
runEditorEffect this eff = do
  state <- getState this
  props <- getProps this
  res <- runExceptT (runReaderT (runStateT eff state) props)
  case res of
    Left err -> do
      -- Console.log $ "[!] " <> err
      modifyState this $ logEditorConsole (stringEditorConsoleError err)
    Right (_ /\ state') -> modifyState this \_ -> state'

setLocation :: Location -> EditorEffect Unit
setLocation loc = do
  modify_ _ { location = loc }

stepLeft :: EditorEffect Unit
stepLeft = do
  state <- get
  case Location.stepLeft state.location of
    Just location' -> put state { location = location' }
    Nothing -> throwError $ "could not step left at location: " <> show state.location

stepRight :: EditorEffect Unit
stepRight = do
  state <- get
  case Location.stepRight state.location of
    Just location' -> put state { location = location' }
    Nothing -> throwError $ "could not step right at location: " <> show state.location

stepDown :: EditorEffect Unit
stepDown = do
  state <- get
  case Location.stepDown state.location of
    Just location' -> put state { location = location' }
    Nothing -> throwError $ "could not step down at location: " <> show state.location

stepUp :: EditorEffect Unit
stepUp = do
  state <- get
  case Location.stepUp state.location of
    Just location' -> put state { location = location' }
    Nothing -> throwError $ "could not step up at location: " <> show state.location
