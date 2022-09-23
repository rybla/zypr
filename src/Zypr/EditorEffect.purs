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
import React (ReactThis, getProps, getState, modifyState)
import Zypr.EditorConsole (logEditorConsole, stringEditorConsoleError, stringEditorConsoleInfo, stringEditorConsoleLog)
import Zypr.EditorTypes (EditorState, EditorProps)
import Zypr.Location (Location)
import Zypr.Location as Location
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
  modify_ _ { location = loc }

stepPrev :: EditorEffect Unit
stepPrev = do
  state <- get
  case Location.stepPrev state.location of
    Just location' -> do
      tell [ "stepped previous" ]
      put state { location = location' }
    Nothing -> throwError $ "can't step left at location: " <> show state.location

stepNext :: EditorEffect Unit
stepNext = do
  state <- get
  case Location.stepNext state.location of
    Just location' -> do
      tell [ "stepped next" ]
      put state { location = location' }
    Nothing -> throwError $ "can't step right at location: " <> show state.location

stepDown :: EditorEffect Unit
stepDown = do
  state <- get
  case Location.stepDown state.location of
    Just location' -> do
      tell [ "stepped down" ]
      put state { location = location' }
    Nothing -> throwError $ "can't step down at location: " <> show state.location

stepUp :: EditorEffect Unit
stepUp = do
  state <- get
  case Location.stepUp state.location of
    Just location' -> do
      tell [ "stepped up " ]
      put state { location = location' }
    Nothing -> throwError $ "can't step up at location: " <> show state.location
