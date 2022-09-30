module Zypr.Static.Intro where

import Prelude
import Data.Array (intercalate, singleton)
import React.DOM as DOM
import React.DOM.Props as Props
import Zypr.SyntaxTheme (Res)

introRes :: Res
introRes =
  let
    em = DOM.em' <<< singleton <<< DOM.text

    i = DOM.i' <<< singleton <<< DOM.text

    a href text = DOM.a [ Props.href href ] [ DOM.text text ]

    ia href text = DOM.i' [ DOM.a [ Props.href href ] [ DOM.text text ] ]

    code text = DOM.span [ Props.className "code" ] [ DOM.text text ]

    codeblock lines = DOM.div [ Props.className "code codeblock" ] [ DOM.text $ intercalate "\n" lines ]
  in
    [ DOM.div [ Props.className "intro" ]
        [ DOM.h3' [ DOM.text "What is Zypr?" ]
        -- 
        , DOM.p' [ em "Zypr", DOM.text " is a zippy (i.e. zipper-based) structural editor. A zipper is data structure that encodes a path from a sub-node of the tree to the top of the tree that also encodes the structure of the tree around the path. The start point of the zipper is called the ", em "clasp", DOM.text " and the end point of the zipper is called the ", em "top", DOM.text " In other words, a zipper looks like a tree with a sub-node missing. In this way, ", i "zipping (up)", DOM.text " the tree corresponds to shortening the path to point to a parent node, and ", i "unzipping (down)", DOM.text " the tree corresponds to extending the path into a child node. For a well-written and more detailed introduction to the concept of a zipper, see Gerard Huet's ", ia "https://dl.acm.org/doi/10.1017/S0956796897002864" "The Zipper", DOM.text "." ]
        -- 
        , DOM.h3' [ DOM.text "Structurally Editing with Zippers" ]
        --
        , DOM.h4' [ DOM.text "Cursor" ]
        , DOM.p'
            [ DOM.text "So how are zippers used to structurally edit? As Huet imagines in "
            , i "The Zipper"
            , DOM.text ", the user's cursor in the program is encoded by a "
            , i "location"
            , DOM.text " which consists of a zipper into the AST and the sub-node that currently resides at that zipper's path. For example, consider the following program."
            , codeblock
                [ "let add1 x = suc x in"
                , "let twice f x = f (f x) in"
                , "twice add1 0"
                ]
            , DOM.text "If the user's cursor is at "
            , code "f x"
            , DOM.text " then their cursor corresponds to the following location,"
            , codeblock
                [ "location:"
                , "  zipper:"
                , "    let add1 x = suc x in"
                , "    let twice f x = f @ in"
                , "    twice add1 0"
                , "  term:"
                , "    f x"
                ]
            , DOM.text "where the "
            , code "@"
            , DOM.text " represents the zipper's clasp. In this way, the user's cursor is at the zipper's clasp."
            ]
        , DOM.p'
            [ DOM.text "This turns out to be a very convenient way to encode the program and the cursor because it allows constant-time modifications of the program at the cursor. The term at the cursor can be wrapped in syntax constructs by building on the zipper, or it can be modified in place by replacing the term and not touching the zipper at all."
            ]
        , DOM.p'
            [ DOM.text "Cursor movement is performed by zipping (going up) and unzipping (going down). Lateral movement can be thought of as zipping and then unzipping down into the original clasp's sibling (of course, it is implemented in zypr more efficiently than this)."
            ]
        -- 
        , DOM.h4' [ DOM.text "Selection" ]
        , DOM.p'
            [ DOM.text "A critical component of editing that Heut's ", i "The Zipper", DOM.text " did not cover was selection. In text editing, a selection is encoded as two cursors, where the substring between them is the selected text. This is exactly how selection is encoded in zypr as well, however, it looks a little different when considered for a zipper cursor as described above."
            ]
        , DOM.p'
            [ DOM.text "In zypr, a selection is encoded by two cursors -- a top cursor and a bottom cursor. The top cursor corresponds to a zipper into the top-level program, and the bottom cursor corresponds to a location in the sub-node that begins as the clasp of the top cursor's zipper. So, the selection itself is the bottom cursor's location's zipper. Note that this encoding only yields selections that are between a node and one of its descendants -- we argue that this is the only kind of selection in trees that is generally consistent. For example, recall the example program from the previous section."
            , codeblock
                [ "let add1 x = suc x in"
                , "let twice f x = f (f x) in"
                , "twice add1 0"
                ]
            , DOM.text "To select just the second "
            , code "let"
            , DOM.text " statement, the zipper selection looks like the following."
            , codeblock
                [ "selection:"
                , "  top zipper:"
                , "    let add1 x = suc x in @"
                , "  bottom location:"
                , "    zipper:"
                , "      let twice f x = f (f x) in @"
                , "    term:"
                , "      twice add1 0"
                ]
            , DOM.text "This selection can be cut/copied, and then pasted by wrapping a cursor with the clibboard by building up the cursor's zipper with the copied zipper."
            ]
        ]
    ]
