module Zypr.EditorEffect where

import Prelude
import Zypr.Syntax
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, get, gets, modify, modify_, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import React (ReactThis, getProps, getState, modifyState)
import Text.PP (pprint)
import Text.PP as PP
import Zypr.EditorConsole (logEditorConsole, stringEditorConsoleError, stringEditorConsoleLog)
import Zypr.EditorTypes (Clipboard, CursorMode, EditorMode(..), EditorProps, EditorState, Query, QueryAction(..), QueryOutput, SelectMode, QueryInput, emptyClipboard, emptyQuery)
import Zypr.Key (Key)
import Zypr.Key as Key
import Zypr.Location (Location)
import Zypr.Location as Location
import Zypr.ModifyString (modifyStringViaKey, modifyStringViaKeyWithResult)
import Zypr.ModifyString as ModifyString
import Zypr.Path (Path(..))
import Zypr.Path as Path
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
  tell [ "load term: " <> PP.pprint term ]
  setMode $ TopMode { term }

setLocation :: Location -> EditorEffect Unit
setLocation loc = do
  tell [ "jump to location: " <> show (Location.ppLocation loc) ]
  state <- get
  setMode
    $ CursorMode
        { location: loc
        , query: emptyQuery
        }

modifyLocation :: (Location -> EditorEffect Location) -> EditorEffect Unit
modifyLocation f = do
  cursor <- requireCursorMode
  loc' <- f cursor.location
  setMode $ CursorMode cursor { location = loc' }

stepPrev :: EditorEffect Unit
stepPrev =
  step \loc -> case Location.stepPrev loc of
    Just loc' -> do
      tell [ "step backwards" ]
      pure loc'
    Nothing -> throwError $ "can't step backward at location: " <> show (Location.ppLocation loc)

stepNext :: EditorEffect Unit
stepNext =
  step \loc -> case Location.stepNext loc of
    Just loc' -> do
      tell [ "step forwards" ]
      pure loc'
    Nothing -> throwError $ "can't step forward at location: " <> show (Location.ppLocation loc)

stepDown :: EditorEffect Unit
stepDown =
  step \loc -> case Location.stepDown loc of
    Just loc' -> do
      tell [ "step down" ]
      pure loc'
    Nothing -> throwError $ "can't step down at location: " <> show (Location.ppLocation loc)

stepUp :: EditorEffect Unit
stepUp =
  step \loc -> case Location.stepUp loc of
    Just loc' -> do
      tell [ "step up " ]
      pure loc'
    Nothing -> throwError $ "can't step up at location: " <> show (Location.ppLocation loc)

stepRight :: EditorEffect Unit
stepRight =
  step \loc -> case Location.stepRight loc of
    Just loc' -> do
      tell [ "step right" ]
      pure loc'
    Nothing -> throwError $ "can't step right at location: " <> show (Location.ppLocation loc)

stepLeft :: EditorEffect Unit
stepLeft =
  step \loc -> case Location.stepLeft loc of
    Just loc' -> do
      tell [ "step right" ]
      pure loc'
    Nothing -> throwError $ "can't step right at location: " <> show (Location.ppLocation loc)

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
      setMode
        $ CursorMode
            { location: { syn: TermSyntax top.term, path: Top }
            , query: emptyQuery
            }
    CursorMode cursor -> do
      location <- f cursor.location
      setMode $ CursorMode cursor { location = location }
    SelectMode select -> do
      locationEnd <- f select.locationEnd
      setMode $ SelectMode select { locationEnd = locationEnd }

toggleConsoleVisible :: EditorEffect Unit
toggleConsoleVisible = modify_ \state -> state { consoleVisible = not state.consoleVisible }

setMode :: EditorMode -> EditorEffect Unit
setMode mode = do
  modify_ _ { mode = mode }

escape :: EditorEffect Unit
escape = do
  state <- get
  -- OLD: when Escape would clear the clipboard
  -- case state.clipboard of
  --   Just _ -> clearClipboard
  --   _ -> do
  --     case state.mode of
  --       CursorMode _ -> escapeCursor
  --       SelectMode _ -> escapeSelect
  --       _ -> pure unit
  case state.mode of
    CursorMode cursor
      | Just _ <- cursor.query.mb_output -> clearQuery
      | otherwise -> escapeCursor
    SelectMode _ -> escapeSelect
    _ -> pure unit

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
      setMode
        $ CursorMode
            { location: select.locationStart
            , query: emptyQuery
            }
    _ -> pure unit

enterSelect :: EditorEffect Unit
enterSelect = do
  state <- get
  case state.mode of
    TopMode top ->
      setMode
        $ CursorMode
            { location: { syn: TermSyntax top.term, path: Top }
            , query: emptyQuery
            }
    CursorMode cursor -> do
      tell [ "enter select" ]
      setMode
        $ SelectMode
            { locationStart: cursor.location
            , locationEnd: { syn: cursor.location.syn, path: Top }
            }
    SelectMode _ -> pure unit

escapeQuery :: EditorEffect Unit
escapeQuery = do
  cursor <- requireCursorMode
  setMode
    $ CursorMode
        cursor
          { query = emptyQuery }

setSyntaxTheme :: SyntaxTheme -> EditorEffect Unit
setSyntaxTheme thm = do
  modify_ _ { syntaxTheme = thm }

requireCursorMode :: EditorEffect CursorMode
requireCursorMode = do
  state <- get
  case state.mode of
    CursorMode cursor -> pure cursor
    _ -> throwError "requires cursor mode"

requireSelectMode :: EditorEffect SelectMode
requireSelectMode = do
  state <- get
  case state.mode of
    SelectMode select -> pure select
    _ -> throwError "requires select mode"

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
  tell [ "enlambda" ]
  modifyTermAtCursor $ pure <<< lam ""
  stepNext

enlet :: EditorEffect Unit
enlet = do
  tell [ "enlet" ]
  modifyTermAtCursor $ pure <<< let_ "" hole
  stepNext

enapp :: EditorEffect Unit
enapp = do
  tell [ "enapp" ]
  modifyTermAtCursor $ pure <<< app hole
  stepNext

enarg :: EditorEffect Unit
enarg = do
  tell [ "enarg" ]
  modifyTermAtCursor $ pure <<< flip app hole
  stepDown
  stepRight

unwrap :: EditorEffect Unit
unwrap = do
  state <- get
  case state.mode of
    CursorMode _ -> do
      tell [ "unwrap at cursor" ]
      modifyTermAtCursor case _ of
        App { apl } -> pure apl
        Lam { bod } -> pure bod
        Let { bod } -> pure bod
        _ -> pure hole
    SelectMode _ -> unwrapSelection
    _ -> throwError "can't unwrap here"

unwrap' :: EditorEffect Unit
unwrap' = do
  state <- get
  case state.mode of
    CursorMode _ -> do
      tell [ "unwrap' at cursor" ]
      modifyTermAtCursor case _ of
        App { arg } -> pure arg
        Lam { bod } -> pure bod
        Let { bod } -> pure bod
        _ -> pure hole
    SelectMode _ -> unwrapSelection
    _ -> throwError "can't unwrap' here "

dig :: EditorEffect Unit
dig = do
  tell [ "dig" ]
  modifyTermAtCursor \_ -> pure hole

editId :: (String -> String) -> EditorEffect Unit
editId f =
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
    _ -> throwError "to edit Id, requires cursor at Var or Bind"

modifyIdViaKey :: Key -> EditorEffect Unit
modifyIdViaKey key = editId (modifyStringViaKey key)

clearId :: EditorEffect Unit
clearId = do
  modifySyntaxAtCursor case _ of
    TermSyntax (Var var) -> pure $ TermSyntax hole
    TermSyntax (Hole _) -> pure $ TermSyntax hole
    BindSyntax (Bind dat) -> pure $ BindSyntax $ Bind dat { id = "" }
    _ -> throwError "to clear, Id requires cursor at Var or Bind"

-- the cursor is at the Location of an editable Syntax
isEditable :: EditorEffect Boolean
isEditable = do
  res <-
    getSyntax
      >>= case _ of
          TermSyntax (Var _) -> pure true
          BindSyntax _ -> pure true
          _ -> pure false
  pure res

{-
-- normal backspace
backspace :: EditorEffect Unit
backspace = do
  state <- get
  case state.mode of
    CursorMode _cursor -> do
      isEditable
        >>= case _ of
            true -> editId "Backspace"
            false -> unwrap
    SelectMode _select -> unwrap
    _ -> throwError "can't backspace here "
-}
backspace :: EditorEffect Unit
backspace = do
  state <- get
  case state.mode of
    CursorMode cursor
      | BindSyntax _ <- cursor.location.syn -> modifyIdViaKey Key.key_Backspace
      | Just _ <- cursor.query.mb_output -> modifyQueryStringViaKey Key.key_Backspace
      | otherwise -> unwrap
    SelectMode _select -> unwrap
    _ -> throwError "can't backspace here "

{-
-- alternative backspace; some Syntax has multiple alternative ways to backspace
backspace' :: EditorEffect Unit
backspace' = do
  state <- get
  case state.mode of
    CursorMode _cursor -> do
      isEditable
        >>= case _ of
            true -> editId "Backspace"
            false -> unwrap'
    SelectMode _select -> unwrap'
    _ -> throwError "can't backspace' here"
-}
-- super backspace; deletes all of focussed Syntax
backspaceSuper :: EditorEffect Unit
backspaceSuper = do
  state <- get
  case state.mode of
    CursorMode _cursor -> do
      isEditable
        >>= case _ of
            true -> clearId
            false -> dig
    SelectMode _select -> unwrap
    _ -> throwError "can't backspaceSuper here"

-- normally, can't put a space into a string using ModifyString utilities, so we
-- have to do it manually here
space :: EditorEffect Unit
space = do
  state <- get
  case state.mode of
    CursorMode cursor -> case cursor.location.syn of
      BindSyntax _ -> pure unit
      TermSyntax _ ->
        if String.null cursor.query.input.string then
          setQueryInputString " "
        else
          throwError "can only put a space in a query at the start"
    _ -> throwError "can't space here"

-- TODO
copy :: EditorEffect Unit
copy = do
  state <- get
  case state.mode of
    CursorMode cursor -> case cursor.location.syn of
      TermSyntax term -> do
        tell [ "copy term: " <> pprint term ]
        setClipboard $ Just $ Left term
      _ -> throwError "can't copy a non-term"
    SelectMode select -> do
      tell [ "copy selection: " <> pprint select.locationEnd.path ]
      setClipboard $ Just $ Right select.locationEnd.path
    _ -> throwError "can't copy without a cursor or selection"

unwrapSelection :: EditorEffect Unit
unwrapSelection = do
  tell [ "unwrap selection" ]
  select <- requireSelectMode
  setMode
    $ CursorMode
        { location:
            { path: select.locationStart.path
            , syn: select.locationEnd.syn
            }
        , query: emptyQuery
        }

setClipboard :: Clipboard -> EditorEffect Unit
setClipboard cb = modify_ \state -> state { clipboard = cb }

clearClipboard :: EditorEffect Unit
clearClipboard = setClipboard emptyClipboard

cut :: EditorEffect Unit
cut = do
  state <- get
  case state.mode of
    CursorMode cursor -> case cursor.location.syn of
      TermSyntax term -> do
        tell [ "cut term: " <> pprint term ]
        setClipboard $ Just $ Left term
        modifyTermAtCursor \_ -> pure hole
      _ -> throwError "can't cut a non-term"
    SelectMode select -> do
      tell [ "cut selection: " <> pprint select.locationEnd.path ]
      setClipboard $ Just $ Right select.locationEnd.path
      setMode
        $ CursorMode
            { location:
                { path: select.locationStart.path
                , syn: select.locationEnd.syn
                }
            , query: emptyQuery
            }
    _ -> throwError "can't cut without a cursor or selection"

paste :: EditorEffect Unit
paste = do
  state <- get
  cursor <- requireCursorMode
  case cursor.location.syn of
    TermSyntax _ -> do
      case state.clipboard of
        -- replace term at cursor
        Just (Left term') -> do
          tell [ "paste term: " <> pprint term' ]
          replaceTermAtCursor term'
        -- wrap around term at cursor 
        Just (Right path) -> do
          tell [ "paste selection: " <> pprint path ]
          wrapTermAtCursor path
        Nothing -> throwError "can't paste with empty clipboard"
    _ -> throwError "can't paste at a non-Term"

replaceTermAtCursor :: Term -> EditorEffect Unit
replaceTermAtCursor term = do
  modifyTermAtCursor \_ -> do
    tell [ "replace term at cursor with term: " <> pprint term ]
    pure term

-- wrap a path around the term at the cursor
wrapTermAtCursor :: Path -> EditorEffect Unit
wrapTermAtCursor path = do
  modifyLocation \loc -> case loc.syn of
    TermSyntax _ -> do
      tell [ "wrap term at cursor with path: " <> pprint path ]
      pure
        $ { path: Path.appendPaths loc.path path
          , syn: loc.syn
          }
    _ -> throwError "can't wrap a path around a non-Term"

keyinput :: Key -> EditorEffect Unit
keyinput key = do
  state <- get
  case state.mode of
    CursorMode cursor -> case cursor.location.syn of
      BindSyntax _ -> editId (modifyStringViaKey key)
      TermSyntax _ -> modifyQueryStringViaKey key
    _ -> pure unit

-- clear query
clearQuery :: EditorEffect Unit
clearQuery = setQueryInputString ""

{-
-- OLD: before key-based string modification
-- modify the query string
modifyQueryString :: (String -> EditorEffect String) -> EditorEffect Unit
modifyQueryString f = do
  cursor <- requireCursorMode
  string' <- f cursor.query.input.string
  let
    input' :: QueryInput
    input' =
      { string: string'
      , ixClasp: 0
      }
  mb_output' <- calculateQuery input'
  setMode
    $ CursorMode
        cursor
          { query =
            cursor.query
              { input = input'
              , mb_output = mb_output'
              }
          }
-}
updateQuery :: QueryInput -> EditorEffect Unit
updateQuery input = do
  cursor <- requireCursorMode
  mb_output <- calculateQuery input
  setMode $ CursorMode cursor { query = { input, mb_output } }

modifyQueryStringViaKey :: Key -> EditorEffect Unit
modifyQueryStringViaKey key = do
  cursor <- requireCursorMode
  let
    { string, mb_result } = modifyStringViaKeyWithResult key cursor.query.input.string
  case mb_result of
    Nothing -> setQueryInputString string
    Just ModifyString.Submit -> submitQuery
    Just ModifyString.Escape -> clearQuery

setQueryInputString :: String -> EditorEffect Unit
setQueryInputString string = updateQuery { string, ixClasp: 0 }

-- move the query clasp index by an int (+1 or -1)
moveQueryIxClasp :: Int -> EditorEffect Unit
moveQueryIxClasp dixClasp = do
  cursor <- requireCursorMode
  case cursor.query.mb_output of
    Nothing -> throwError "can't move query input clasp without active query"
    Just output -> do
      let
        ixClasp = (cursor.query.input.ixClasp + dixClasp) `mod` output.nClasps
      tell [ "changed query.input.ixClasp from " <> show cursor.query.input.ixClasp <> " to " <> show ixClasp ]
      updateQuery cursor.query.input { ixClasp = ixClasp }

moveQueryIxClaspNext :: EditorEffect Unit
moveQueryIxClaspNext = do
  tell [ "move query clasp to next position" ]
  moveQueryIxClasp 1

moveQueryIxClaspPrev :: EditorEffect Unit
moveQueryIxClaspPrev = do
  tell [ "move query clasp to previous position" ]
  moveQueryIxClasp (-1)

-- `string` is query string 
calculateQuery :: QueryInput -> EditorEffect (Maybe QueryOutput)
calculateQuery input = do
  tell [ "calculating query from input: " <> show input ]
  if String.null input.string then
    pure Nothing
  else if input.string `elem` [ "fun", "lam" ] then do
    change <-
      Right
        <$> case input.ixClasp of
            -- clasp at bod
            0 -> pure $ Path.lam_bod (bnd "") Top
            -- bad clasp index
            _ -> throwError "clasp index out of bounds while calculating query"
    pure
      $ Just
          { action: LamQueryAction
          , nClasps: 1
          , change
          }
  -- app
  else if input.string == " " then do
    change <-
      Right
        <$> case input.ixClasp of
            -- clasp at apl
            1 -> pure $ Path.app_apl hole Top
            -- clasp at arg
            0 -> pure $ Path.app_arg hole Top
            -- bad clasp index
            _ -> throwError "clasp index out of bounds while calculating query"
    pure
      $ Just
          { action: AppQueryAction
          , nClasps: 2
          , change
          }
  else if input.string == "let" then do
    change <-
      Right
        <$> case input.ixClasp of
            -- clasp at imp
            1 -> pure $ Path.let_imp (bnd "") hole Top
            -- clasp at bod
            0 -> pure $ Path.let_bod (bnd "") hole Top
            -- bad clasp index
            _ -> throwError "clasp index out of bounds while calculating query"
    pure
      $ Just
          { action: LetQueryAction
          , nClasps: 2
          , change
          }
  else do
    let
      id = idFromString input.string
    pure
      $ Just
          { action: VarQueryAction id
          , nClasps: 0
          , change: Left (var id)
          }

-- submit query
submitQuery :: EditorEffect Unit
submitQuery = do
  cursor <- requireCursorMode
  case cursor.query.mb_output of
    Nothing -> throwError "can't submit query with no active query"
    Just output -> case output.change of
      Left term -> replaceTermAtCursor term
      Right path -> wrapTermAtCursor path
  clearQuery

-- arrows
arrowright :: EditorEffect Unit
arrowright = do
  escapeSelect
  state <- get
  case state.mode of
    CursorMode cursor
      | Just _ <- cursor.query.mb_output -> moveQueryIxClaspNext
    _ -> stepNext

arrowleft :: EditorEffect Unit
arrowleft = do
  escapeSelect
  state <- get
  case state.mode of
    CursorMode cursor
      | Just _ <- cursor.query.mb_output -> moveQueryIxClaspPrev
    _ -> stepPrev

shiftArrowright :: EditorEffect Unit
shiftArrowright = do
  enterSelect
  stepNext

shiftArrowleft :: EditorEffect Unit
shiftArrowleft = do
  enterSelect
  stepPrev
