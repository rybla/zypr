module Zypr.EditorEffect where

import Data.Tuple.Nested
import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, get, modify_, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception.Unsafe (unsafeThrow)
import React (ReactThis, getProps, getState, modifyState)
import Zypr.EditorConsole (logEditorConsole, stringEditorConsoleError, stringEditorConsoleInfo, stringEditorConsoleLog)
import Zypr.EditorTypes (EditorMode(..), EditorProps, EditorState)
import Zypr.Location (Location)
import Zypr.Location as Location
import Zypr.Path (Path(..))
import Zypr.SyntaxTheme (Res)

type EditorEffect a
  = StateT EditorState (ReaderT EditorProps (WriterT (Array String) (ExceptT String Effect))) a

runEditorEffect :: ReactThis EditorProps EditorState -> EditorEffect Unit -> Effect Unit
runEditorEffect this eff = do
  state <- getState this
  props <- getProps this
  res <- runExceptT (runWriterT (runReaderT (runStateT eff state) props))
  case res of
    Left err -> do
      modifyState this $ logEditorConsole (stringEditorConsoleError err)
    Right ((_ /\ state') /\ infos) -> do
      modifyState this
        $ (foldr (<<<) identity (map (logEditorConsole <<< stringEditorConsoleLog) infos))
        <<< \_ -> state'

setLocation :: Location -> EditorEffect Unit
setLocation loc = do
  tell [ "jumped to new location: " <> show loc ]
  modify_ _ { mode = CursorMode { location: loc } }

stepPrev :: EditorEffect Unit
stepPrev =
  step \loc -> case Location.stepPrev loc of
    Just loc' -> do
      tell [ "stepped previous" ]
      pure loc'
    Nothing -> throwError $ "can't step left at location: " <> show loc

stepNext :: EditorEffect Unit
stepNext =
  step \loc -> case Location.stepNext loc of
    Just loc' -> do
      tell [ "stepped next" ]
      pure loc'
    Nothing -> throwError $ "can't step right at location: " <> show loc

stepDown :: EditorEffect Unit
stepDown =
  step \loc -> case Location.stepDown loc of
    Just loc' -> do
      tell [ "stepped down" ]
      pure loc'
    Nothing -> throwError $ "can't step down at location: " <> show loc

stepUp :: EditorEffect Unit
stepUp =
  step \loc -> case Location.stepUp loc of
    Just loc' -> do
      tell [ "stepped up " ]
      pure loc'
    Nothing -> throwError $ "can't step up at location: " <> show loc

step :: (Location -> EditorEffect Location) -> EditorEffect Unit
step f = do
  state <- get
  case state.mode of
    CursorMode cursor -> do
      location <- f cursor.location
      put state { mode = CursorMode cursor { location = location } }
    SelectMode select -> do
      locationEnd <- f select.locationEnd
      put state { mode = SelectMode select { locationEnd = locationEnd } }

exitSelect :: EditorEffect Unit
exitSelect = do
  state <- get
  case state.mode of
    CursorMode _ -> pure unit
    SelectMode select -> do
      tell [ "exit select" ]
      put state { mode = CursorMode { location: select.locationStart } }

enterSelect :: EditorEffect Unit
enterSelect = do
  state <- get
  case state.mode of
    CursorMode cursor -> do
      tell [ "enter select" ]
      put
        state
          { mode =
            SelectMode
              { locationStart: cursor.location
              , locationEnd: { term: cursor.location.term, path: Top }
              }
          }
    SelectMode _ -> pure unit
