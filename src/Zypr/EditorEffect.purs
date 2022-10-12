module Zypr.EditorEffect where

import Prelude
import Zypr.EditorTypes
import Zypr.Syntax

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, get, gets, modify, modify_, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (elem, length, reverse, (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr, sequence_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import React (ReactThis, getProps, getState, modifyState)
import Text.PP (pprint)
import Text.PP as PP
import Zypr.EditorConsole (logEditorConsole, stringEditorConsoleError, stringEditorConsoleLog)
import Zypr.Indent (toggleIndentData)
import Zypr.Key (Key)
import Zypr.Key as Key
import Zypr.Location (Location, wrapPath)
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
  pushHistory cursor.location
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

-- steps the upper part of a selection up one
stepPrevPath :: EditorEffect Unit
stepPrevPath = pure unit
-- get two paths
-- 

-- skips binds
stepNextTerm :: EditorEffect Unit
stepNextTerm = do
  stepNext
  state <- get
  case state.mode of
    CursorMode cursor
      | BindSyntax _ <- cursor.location.syn -> stepNextTerm
    SelectMode select
      | BindSyntax _ <- select.locationEnd.syn -> stepNextTerm
    _ -> pure unit

-- skips terms
stepPrevTerm :: EditorEffect Unit
stepPrevTerm = do
  stepPrev
  state <- get
  case state.mode of
    CursorMode cursor
      | BindSyntax _ <- cursor.location.syn -> stepPrevTerm
    SelectMode select
      | BindSyntax _ <- select.locationEnd.syn -> stepPrevTerm
    _ -> pure unit

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

stepNextHole :: EditorEffect Unit
stepNextHole = do
  step \loc -> case Location.stepNextHole loc of
    Just loc' -> do
      tell [ "step next hole" ]
      pure loc'
    Nothing -> throwError $ "can't find a next hole"

stepPrevHole :: EditorEffect Unit
stepPrevHole = do
  step \loc -> case Location.stepPrevHole loc of
    Just loc' -> do
      tell [ "step previous hole" ]
      pure loc'
    Nothing -> throwError $ "can't find a previous hole"

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
      clearQuery
    SelectMode select -> do
      locationEnd <- f select.locationEnd
      setMode $ SelectMode select { locationEnd = locationEnd }

toggleConsoleVisible :: EditorEffect Unit
toggleConsoleVisible = modify_ \state -> state { consoleVisible = not state.consoleVisible }

toggleIntroVisible :: EditorEffect Unit
toggleIntroVisible = modifyIntroVisible not

modifyIntroVisible :: (Boolean -> Boolean) -> EditorEffect Unit
modifyIntroVisible f = modify_ \state -> state { introVisible = f state.introVisible }

toggleHelpVisible :: EditorEffect Unit
toggleHelpVisible = modifyHelpVisible not

modifyHelpVisible :: (Boolean -> Boolean) -> EditorEffect Unit
modifyHelpVisible f = modify_ \state -> state { helpVisible = f state.helpVisible }

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
            { location: {path : select.pathStart, syn: wrapPath select.locationEnd.path select.locationEnd.syn}
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
            { pathStart: cursor.location.path
            , locationEnd: { syn: cursor.location.syn, path: Top }
            , cursorAtTopPath : false
            }
    SelectMode _ -> pure unit

enterCursor :: EditorEffect Unit
enterCursor = do
  state <- get
  case state.mode of
    TopMode top ->
      setMode
        $ CursorMode
            { location: { syn: TermSyntax top.term, path: Top }
            , query: emptyQuery
            }
    CursorMode _ -> pure unit
    SelectMode select ->
      setMode
        $ CursorMode
            { location: {path: select.pathStart, syn: wrapPath select.locationEnd.path select.locationEnd.syn}
            , query: emptyQuery
            }

-- start a query initialized with the var's (at cursor) id
enterQueryVar :: EditorEffect Unit
enterQueryVar = do
  cursor <- requireCursorMode
  case cursor.location.syn of
    TermSyntax (Var var) -> do
      setQueryInputString var.dat.id
      setQueryDisplayOldTerm false
    _ -> throwError "can't enter query with Var's Id at a non-Var"

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
  pushHistory cursor.location
  setMode
    $ CursorMode cursor { location { syn = syn' } }

modifyTermAtCursor :: (Term -> EditorEffect Term) -> EditorEffect Unit
modifyTermAtCursor f = do
  cursor <- requireCursorMode
  case cursor.location.syn of
    TermSyntax term -> do
      term' <- f term
      pushHistory cursor.location
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
      | BindSyntax _ <- cursor.location.syn -> do
        modifyIdViaKey Key.key_Backspace
      | TermSyntax term <- cursor.location.syn
      , not (String.null cursor.query.input.string) -> do
        modifyQueryStringViaKey Key.key_Backspace
        case term of
          -- if deleted entire id of var, then unwrap
          Var var -> do
            cursor' <- requireCursorMode
            when (String.null cursor'.query.input.string) unwrap
          _ -> pure unit
      -- OLD: when typing at Var immediately entered query with Var Id
      -- -- special case for backspacing at var
      -- | TermSyntax (Var var) <- cursor.location.syn
      -- , String.null cursor.query.input.string -> do
      --   setQueryInputString var.dat.id
      --   modifyQueryStringViaKey Key.key_Backspace
      --   cursor' <- requireCursorMode
      --   when (String.null cursor'.query.input.string) unwrap
      | otherwise -> unwrap
    SelectMode _select -> unwrap
    _ -> throwError "can't backspace here"

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
      BindSyntax _ -> stepNextHole -- TODO: Henry's idea
      TermSyntax _ ->
        if String.null cursor.query.input.string then
          setQueryInputString " "
        else
          -- throwError "Space is not allowed in a query other than as the first char"
          submitQuery -- TODO: Henry's idea
    _ -> throwError "can't space here"

enter :: EditorEffect Unit
enter = do
  state <- get
  case state.mode of
    CursorMode cursor
      | String.null cursor.query.input.string
      , TermSyntax (Var _) <- cursor.location.syn -> enterQueryVar
      | not (String.null cursor.query.input.string) -> submitQuery
    _ -> throwError "can't Enter here"

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
            { path: select.pathStart
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
                {path: select.pathStart, syn: wrapPath select.locationEnd.path select.locationEnd.syn}
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
      BindSyntax _bind -> editId (modifyStringViaKey key)
      -- OLD: when typing at Var immediately entered query with Var Id
      -- TermSyntax term
      --   -- special case for typing at a var
      --   | Var var <- term
      --   , String.null cursor.query.input.string -> do
      --     setQueryInputString var.dat.id
      --     modifyQueryStringViaKey key
      --   -- typing at any other term
      --   | otherwise -> modifyQueryStringViaKey key
      TermSyntax term
        -- if querying at Var, make sure to still show old Var
        | Var _ <- term, String.null cursor.query.input.string -> do
          setQueryDisplayOldTerm true
          modifyQueryStringViaKey key
        -- if querying at non-Var, then display of old term is already handled
        | otherwise -> modifyQueryStringViaKey key
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
updateQuery :: (QueryInput -> QueryInput) -> EditorEffect Unit
updateQuery f = do
  cursor <- requireCursorMode
  let
    input = f cursor.query.input
  mb_output <- calculateQuery input
  tell
    [ "new query input " <> show input <> " yields new query "
        <> case mb_output <#> _.change of
            Just (Left term) -> "output term: " <> pprint term
            Just (Right path) -> "output path: " <> pprint path
            Nothing -> "output: Nothing"
    ]
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
setQueryInputString string =
  updateQuery
    _
      { string = string
      , ixClasp = 0
      }

-- move the query clasp index by an int (+1 or -1)
moveQueryIxClasp :: Int -> EditorEffect Unit
moveQueryIxClasp dixClasp = do
  cursor <- requireCursorMode
  case cursor.query.mb_output of
    Nothing -> throwError "can't move query input clasp without active query"
    Just output -> do
      let
        ixClasp = (cursor.query.input.ixClasp + dixClasp) `mod` output.nClasps
      updateQuery _ { ixClasp = ixClasp }

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
          { nClasps: 1
          , change
          }
  -- app
  else if input.string == " " then do
    change <-
      Right
        <$> case input.ixClasp of
            -- clasp at apl
            0 -> pure $ Path.app_apl hole Top
            -- clasp at arg
            1 -> pure $ Path.app_arg hole Top
            -- bad clasp index
            _ -> throwError "clasp index out of bounds while calculating query"
    pure
      $ Just
          { nClasps: 2
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
          { nClasps: 2
          , change
          }
  else if input.string == "+" then do
    change <-
      Right
        <$> case input.ixClasp of
            -- clasp at left
            0 -> pure $ Path.plus_left hole Top
            -- clasp at right
            1 -> pure $ Path.plus_right hole Top
            -- bad clasp index
            _ -> throwError "clasp index out of bounds while calculating query"
    pure
      $ Just
          { nClasps: 2
          , change
          }
  else do
    let
      id = idFromString input.string
    pure
      $ Just
          { nClasps: 0
          , change: Left (var id)
          }

-- submit query
submitQuery :: EditorEffect Unit
submitQuery = do
  cursor <- requireCursorMode
  case cursor.query.mb_output of
    Nothing -> throwError "can't submit query with no active query"
    Just output -> do
      tell [ "submit query" ]
      case output.change of
        Left term -> replaceTermAtCursor term
        Right path -> do
          wrapTermAtCursor path
          -- where to move cursor after submitting
          case path of
            Zip { dat: TermData (LamData _) } -> sequence_ [ stepUp, stepNext ]
            Zip { dat: TermData (AppData _) } -> stepUp
            Zip { dat: TermData (LetData _) } -> sequence_ [ stepUp, stepNext ]
            _ -> pure unit
  clearQuery

setQueryDisplayOldTerm :: Boolean -> EditorEffect Unit
setQueryDisplayOldTerm displayOldTerm =
  updateQuery
    _
      { displayOldTerm = displayOldTerm }

-- arrows
arrowright :: EditorEffect Unit
arrowright = do
  escapeSelect
  state <- get
  case state.mode of
    SelectMode select -> stepNextTerm
    CursorMode cursor
      | Just _ <- cursor.query.mb_output -> moveQueryIxClaspNext
    _ -> stepNext

arrowleft :: EditorEffect Unit
arrowleft = do
  escapeSelect
  state <- get
  case state.mode of
    SelectMode select -> stepPrevTerm
    CursorMode cursor
      | Just _ <- cursor.query.mb_output -> moveQueryIxClaspPrev
    _ -> stepPrev

-- If in selection mode and the selection is empty, moves into cursor mode
checkIfEmptySelection :: EditorEffect Unit
checkIfEmptySelection = do
  state <- get
  case state.mode of
    SelectMode select -> if select.locationEnd.path == Top then
        setMode $ CursorMode
            { location: {path: select.pathStart, syn: select.locationEnd.syn}
            , query: emptyQuery
            }
        else pure unit
    _ -> pure unit

shiftArrowright :: EditorEffect Unit
shiftArrowright = do
  enterSelect
  stepNextTerm
  checkIfEmptySelection

shiftArrowleft :: EditorEffect Unit
shiftArrowleft = do
  enterSelect
  stepPrevTerm
  checkIfEmptySelection

toggleIndent :: EditorEffect Unit
toggleIndent = do
  cursor <- requireCursorMode
  -- modifyTermAtCursor \term -> ?a 
  -- case Location.stepUp cursor.location of
  --   Nothing -> throwError "can't indent top of program"
  --   Just loc -> setLocation loc
  case cursor.location.path of
    Top -> throwError "can't indent top of program"
    Zip { dat, lefts, up, rights } ->
      let
        dat' = case toggleIndentData dat (length lefts) of
          Just dat' -> dat'
          Nothing -> dat
      in
        setLocation
          { syn: cursor.location.syn
          , path: Zip { dat: dat', lefts, up, rights }
          }

pushHistory :: Location -> EditorEffect Unit
pushHistory loc =
  modify_ \state ->
    state
      { history = loc : Array.take 10 state.history }

popHistory :: EditorEffect Location
popHistory = do
  state <- get
  case Array.uncons state.history of
    Just { head: loc, tail: history' } -> do
      modify_ _ { history = history' }
      pure loc
    Nothing -> throwError "Can't go back earlier than the beginning of history."

undo :: EditorEffect Unit
undo = do
  loc <- popHistory
  setLocation loc
