module Zypr.EditorEffect where

import Prelude
import Zypr.Syntax
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, get, modify, modify_, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React (ReactThis, getProps, getState, modifyState)
import Text.PP as PP
import Zypr.EditorConsole (logEditorConsole, stringEditorConsoleError, stringEditorConsoleLog)
import Zypr.EditorTypes (EditorMode(..), EditorProps, EditorState, CursorMode)
import Zypr.Location (Location, ppLocation)
import Zypr.Location as Location
import Zypr.Path (Path(..))
import Zypr.SyntaxTheme (SyntaxTheme)

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

getSyntax :: EditorEffect Syntax
getSyntax = do
  cursor <- requireCursorMode
  pure cursor.location.syn

setTerm :: Term -> EditorEffect Unit
setTerm term = do
  tell [ "loaded term: " <> PP.pprint term ]
  setMode $ TopMode { term }

setLocation :: Location -> EditorEffect Unit
setLocation loc = do
  tell [ "jumped to location: " <> show (ppLocation loc) ]
  state <- get
  setMode $ CursorMode { location: loc }

stepPrev :: EditorEffect Unit
stepPrev =
  step \loc -> case Location.stepPrev loc of
    Just loc' -> do
      tell [ "stepped previous" ]
      pure loc'
    Nothing -> throwError $ "can't step backward at location: " <> show (ppLocation loc)

stepNext :: EditorEffect Unit
stepNext =
  step \loc -> case Location.stepNext loc of
    Just loc' -> do
      tell [ "stepped next" ]
      pure loc'
    Nothing -> throwError $ "can't step forward at location: " <> show (ppLocation loc)

stepDown :: EditorEffect Unit
stepDown =
  step \loc -> case Location.stepDown loc of
    Just loc' -> do
      tell [ "stepped down" ]
      pure loc'
    Nothing -> throwError $ "can't step down at location: " <> show (ppLocation loc)

stepUp :: EditorEffect Unit
stepUp =
  step \loc -> case Location.stepUp loc of
    Just loc' -> do
      tell [ "stepped up " ]
      pure loc'
    Nothing -> throwError $ "can't step up at location: " <> show (ppLocation loc)

stepRight :: EditorEffect Unit
stepRight =
  step \loc -> case Location.stepRight loc of
    Just loc' -> do
      tell [ "stepped right" ]
      pure loc'
    Nothing -> throwError $ "can't step right at location: " <> show (ppLocation loc)

stepLeft :: EditorEffect Unit
stepLeft =
  step \loc -> case Location.stepLeft loc of
    Just loc' -> do
      tell [ "stepped right" ]
      pure loc'
    Nothing -> throwError $ "can't step right at location: " <> show (ppLocation loc)

stepRoot :: EditorEffect Unit
stepRoot = do
  let
    go = do
      state <- get
      case state.mode of
        CursorMode cursor
          | Top <- cursor.location.path -> pure unit
          | otherwise -> do
            stepUp
            go
        _ -> pure unit
  go

step :: (Location -> EditorEffect Location) -> EditorEffect Unit
step f = do
  state <- get
  case state.mode of
    TopMode top -> do
      setMode $ CursorMode { location: { syn: TermSyntax top.term, path: Top } }
    CursorMode cursor -> do
      location <- f cursor.location
      setMode $ CursorMode cursor { location = location }
    SelectMode select -> do
      locationEnd <- f select.locationEnd
      setMode $ SelectMode select { locationEnd = locationEnd }

setMode :: EditorMode -> EditorEffect Unit
setMode mode = do
  modify_ _ { mode = mode }

escapeCursor :: EditorEffect Unit
escapeCursor = do
  state <- get
  case state.mode of
    CursorMode cursor -> do
      stepRoot
      state <- get
      case state.mode of
        CursorMode cursor'
          | Top <- cursor'.location.path
          , TermSyntax term <- cursor'.location.syn -> do
            tell [ "escape cursor" ]
            setMode $ TopMode { term }
          | otherwise ->
            throwError
              $ "escapeCursor: stepRoot ended at non-Top Path: "
              <> PP.pprint cursor'.location.path
        _ -> pure unit
    _ -> pure unit

escapeSelect :: EditorEffect Unit
escapeSelect = do
  state <- get
  case state.mode of
    SelectMode select -> do
      tell [ "escape select" ]
      setMode $ CursorMode { location: select.locationStart }
    _ -> pure unit

enterSelect :: EditorEffect Unit
enterSelect = do
  state <- get
  case state.mode of
    TopMode top ->
      setMode
        $ CursorMode { location: { syn: TermSyntax top.term, path: Top } }
    CursorMode cursor -> do
      tell [ "enter select" ]
      setMode
        $ SelectMode
            { locationStart: cursor.location
            , locationEnd: { syn: cursor.location.syn, path: Top }
            }
    SelectMode _ -> pure unit

setSyntaxTheme :: SyntaxTheme -> EditorEffect Unit
setSyntaxTheme thm = do
  modify_ _ { syntaxTheme = thm }

requireCursorMode :: EditorEffect CursorMode
requireCursorMode = do
  state <- get
  case state.mode of
    CursorMode cursor -> pure cursor
    _ -> throwError "requires cursor mode"

modifySyntaxAtCursor :: (Syntax -> EditorEffect Syntax) -> EditorEffect Unit
modifySyntaxAtCursor f = do
  cursor <- requireCursorMode
  syn' <- f cursor.location.syn
  setMode
    $ CursorMode cursor { location { syn = syn' } }

modifyTermAtCursor :: (Term -> EditorEffect Term) -> EditorEffect Unit
modifyTermAtCursor f = do
  cursor <- requireCursorMode
  case cursor.location.syn of
    TermSyntax term -> do
      term' <- f term
      setMode $ CursorMode cursor { location { syn = TermSyntax term' } }
    _ -> throwError "requires cursor at a Term"

modifyBindAtCursor :: (Bind -> EditorEffect Bind) -> EditorEffect Unit
modifyBindAtCursor f = do
  cursor <- requireCursorMode
  case cursor.location.syn of
    BindSyntax bnd -> do
      bnd' <- f bnd
      setMode $ CursorMode cursor { location { syn = BindSyntax bnd' } }
    _ -> throwError "requires cursor at a Bind"

enlambda :: EditorEffect Unit
enlambda = do
  modifyTermAtCursor $ pure <<< lam ""
  stepNext

enlet :: EditorEffect Unit
enlet = do
  modifyTermAtCursor $ pure <<< let_ "" hole
  stepNext

enapp :: EditorEffect Unit
enapp = do
  modifyTermAtCursor $ pure <<< app hole
  stepNext

enarg :: EditorEffect Unit
enarg = do
  modifyTermAtCursor $ pure <<< flip app hole
  stepDown
  stepRight

unwrap :: EditorEffect Unit
unwrap =
  modifyTermAtCursor case _ of
    Lam { bod } -> pure bod
    Let { bod } -> pure bod
    _ -> pure hole

dig :: EditorEffect Unit
dig = modifyTermAtCursor \_ -> pure hole

editId :: String -> EditorEffect Unit
editId label = do
  tell [ "label: " <> label ]
  let
    f :: String -> String
    f str = case label of
      "Backspace" -> String.take (max 0 $ String.length str - 1) str
      _ -> str <> label
  modifySyntaxAtCursor case _ of
    TermSyntax (Var var) -> do
      let
        id' = f var.dat.id
      if String.null id' then
        pure $ TermSyntax hole
      else
        pure $ TermSyntax (Var var { dat { id = id' } })
    TermSyntax (Hole _) -> do
      let
        id' = f ""
      if String.null id' then
        pure $ TermSyntax hole
      else
        pure $ TermSyntax (var id')
    BindSyntax (Bind dat) -> do
      pure $ BindSyntax (Bind dat { id = f dat.id })
    _ -> throwError "requires cursor at Var or Bind"

-- the cursor is at the Location of an editable Syntax
isEditable :: EditorEffect Boolean
isEditable = do
  res <-
    getSyntax
      >>= case _ of
          TermSyntax (Var _) -> pure true
          BindSyntax _ -> pure true
          _ -> pure false
  tell [ "isEditable: " <> show res ]
  pure res
