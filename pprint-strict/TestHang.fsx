// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#load "PPrint\Strict.fs"

open PPrint.Strict

let output (doc:Doc) : unit = printDoc 999 doc

let node (name : string) (kids : Doc list) : Doc = 
    match kids with
    | [] -> brackets (text name)
    | _ ->  hang 4 (brackets  (text name ^!!^ vcat kids))

let tree1 : Doc = 
    node "factx-fsharp"
        [ node "src" 
            [ node "bin" []
            ; node "FactX" 
                [ node "Common.fs" []
                ; node "FactOutput.fs" []
                ; node "FactWriter.fs" []
                ; node "Pretty.fs" []
                ; node "Syntax.fs" []
                ]
            ]
        ; node "test" 
            [ node "FactXTest.fsproj" []
            ; node "Test01.fsx" []
            ]
        ]

/// This should print with nice 'expected' indentation indicating nesting.
let demo01 () = output tree1
    
