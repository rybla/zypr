module Zypr.Static.Help where

import Prelude
import Data.Array (intercalate, singleton)
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.SyntaxTheme (Res)

helpRes :: Res
helpRes =
  let
    em = DOM.em' <<< singleton <<< DOM.text

    i = DOM.i' <<< singleton <<< DOM.text

    a href text = DOM.a [ Props.href href ] [ DOM.text text ]

    ia href text = DOM.i' [ DOM.a [ Props.href href ] [ DOM.text text ] ]

    code text = DOM.span [ Props.className "code" ] [ DOM.text text ]

    codeblock lines = DOM.div [ Props.className "code codeblock" ] [ DOM.text $ intercalate "\n" lines ]

    ul = DOM.ul' <<< map (DOM.li' <<< singleton <<< DOM.text)
  in
    [ DOM.h3' [ DOM.text "How to Structurally Edit with Zypr" ]
    , DOM.p' [ DOM.text "It is highly recommended to read the introduction (click on the menu item called 'intro') before reading the rest of this document." ]
    , DOM.h4' [ DOM.text "Cursor" ]
    , DOM.p' [ DOM.text "The cursor is a location in the program, with the program considered as an AST. You can navigate left, right with ", code "ArrowLeft, ArrowRight", DOM.text " respectively. The cursor traverses the tree in a way that can reach any node. Clicking on a node will also navigate the cursor to that node. ", code "Escape", DOM.text " while in cursor mode will remove the cursor and enter top mode. To enter cursor mode again, just start navigating." ]
    , DOM.h4' [ DOM.text "Selection" ]
    , DOM.p' [ DOM.text "While in cursor mode, a selection can be started with ", code "Shift+ArrowLeft, Shift+ArrowRight", DOM.text " which enters select mode. The top zipper of the selection is where the cursor was when select mode was entered. The bottom location of the select is controlled like a cursor with usual navigation, but cannot go outside the term at the top zipper." ]
    , DOM.h4' [ DOM.text "Editing" ]
    , DOM.p'
        [ DOM.text "Editing is performed at a cursor. To edit a node, navigate the cursor to the node and then start typing, which begins a query at that node."
        , ul
            [ "Note that if the node is a variable binding, then typing modifies the binding name rather than starting a query."
            , "Note that if the node was a variable, then the query is initialized with that variable's name."
            ]
        , DOM.text "If the query matches a defined edit, then a temporary application of that edit will be displayed inline. Note that if the query has no clasp and the node is a variable or hole, then the current term is not displayed. Use "
        , code "ArrowLeft, ArrowRight"
        , DOM.text " to change with which clasp the edit action will be applied (if there are multiple). If the query does not match a defined edit, then the query is assumed to be a variable name. "
        , code "Enter"
        , DOM.text "submits the current query. If the queried edit had a clasp, then the cursor is wrapped at that clasp. If the edit did not have a clasp (e.g. entering a variable name), then the term at the cursor is replaced by the new term."
        ]
    , DOM.p' [ DOM.text "In cursor mode, ", code "Backspace", DOM.text " deletes the outermost construct of the term at cursor, or deletes a character of the binding's variable name at the cursor. Additionally, ", code "Ctrl+Backspace", DOM.text " deletes the entire term at the cursor, or deletes the binding's entire variable name at the cursor." ]
    , DOM.p' [ DOM.text "In select mode, ", code "Backspace", DOM.text " deletes the selected zipper, replacing the selection's top zipper with the bottom zipper's term." ]
    , DOM.h4' [ DOM.text "Clipboard" ]
    , DOM.p' [ DOM.text "The clipboard can store either a term or a zipper." ]
    , DOM.p'
        [ DOM.text "Copy a term to the clipboard by naviating the cursor to that term, then ", code "Ctrl+c, Ctrl+x", DOM.text " which copies, cuts respectively. Paste the term in place of another term by navigating to the other term, then ", code "Ctrl+v", DOM.text "."
        ]
    , DOM.p'
        [ DOM.text "Copy a zipper to the clipboard by selecting that zipper, then ", code "Ctrl+c, Ctrl+x", DOM.text " which copies, cuts respectively. Paste the zipper around a term  by navigating to that term, then ", code "Ctrl+v", DOM.text "."
        ]
    , DOM.h4' [ DOM.text "Debug" ]
    , DOM.p' [ code "Ctrl+`", DOM.text " to open the console." ]
    ]
