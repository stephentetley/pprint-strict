// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an implementation of Daan Leijen's PPrint. 
// The original Haskell library was strictified for Mercury by 
// Ralph Becket and subsequently ported to Racket by David 
// Herman.
// The CPS transformation of layout and other functions in this
// implementation is new. 
// Any mistakes are mine (SPT).

// Note always build as Release otherwise tail call optimization
// will be turned off.

// This code has appeared in SLFormat.Pretty - here we implement layout\best without CPS

namespace PPrint


module Strict =
    
    open System
    open System.Text
    
    /// The abstract type of Documents.
    type Doc = 
        private 
            | Nil
            | Text of string
            | Line of bool
            | Cat of Doc * Doc
            | Nest of int * Doc
            | Group of Doc
            | Column of (int -> Doc)
            | Nesting of (int -> Doc)   

    /// The type representing a rendered Doc.
    type private SimpleDoc = 
        | SEmpty 
        | SText of string * SimpleDoc          
        | SLine of string * SimpleDoc     




    let private extend (s:string) (spaces:int) : string = 
        s + new String(c = ' ', count = spaces)

    /// We eliminate Column(f) and Nesting(f) by evaluating flatten with 
    /// column position and nesting level unlike the original PPrint flatten.
    ///
    /// My understanding is that we don't need to supply initial nest level
    /// as it is already accommodated by text padding in 'layout'.
    let private flatten (columnPos:int) (document:Doc) : Doc = 
        let rec work (column:int) (nest:int) (doc:Doc) (cont : int -> Doc -> int * Doc) : (int * Doc) = 
            printfn "flatten - column:%i, nest:%i" column nest
            match doc with
            | Cat(x,y) -> 
                work column nest x (fun c1 x1 -> 
                work c1 nest y (fun c2 y1 -> 
                cont c2 (Cat(x1, y1))))
            | Nest(n,x) -> work column (nest+n) x cont
            | Line(true) -> cont column Nil
            | Line(false) -> cont (column+1) (Text(" "))
            | Group(x) -> work column nest x cont
            | Column(f) -> 
                work column nest (f column) cont
            | Nesting(f) -> 
                work column nest (f nest) cont
            | Text s -> cont (column + s.Length) doc
            | Nil -> cont column Nil
        work columnPos 0 document (fun c x -> (c,x)) |> snd


    /// Warning this is exposed for test purposes (it may disappear in future).
    let internalFlatten (document:Doc) : Doc = flatten 0 document

    let private isTooBig (text:string) (col:int) (width:int) : bool = 
        col + text.Length > width

    exception Backtrack

    let private layout (width:int) (doc:Doc) : SimpleDoc = 
        let rec best (col:int) (docs: list<string * Doc>) (alternate:bool) : SimpleDoc =
            printfn "best - col:%i" col
            match docs with
            | [] -> SEmpty
            | (_, Nil) :: rest ->
                best col rest alternate
            | (iz, Cat(x,y)) :: rest -> 
                printf "CAT (iz:'%s'): " iz
                best col ((iz,x) :: (iz,y) :: rest) alternate
            | (iz, Nest(n,x)) :: rest -> 
                printf "NEST (iz:'%s'): " iz
                best col ((extend iz n,x) :: rest) alternate
            | (iz, Line _) :: rest ->
                printf "LINE (iz:'%s'): " iz
                SLine(iz, best iz.Length rest alternate)
            | (iz, Group(x)) :: rest ->
                try
                    best col ((iz, flatten col x) :: rest) true 
                with
                | Backtrack -> 
                    best col ((iz, x) :: rest) alternate
            | (iz, Text(t)) :: rest ->
                printf "TEXT (%s) : " t
                if (width >= 0) && alternate && isTooBig t col width then
                    raise Backtrack
                else
                    SText(t, best (col + t.Length) rest alternate)
            | (iz, Column(f)) :: rest ->
                best col ((iz, f col) :: rest) alternate
            | (iz, Nesting(f)) :: rest ->
                best col ((iz, f iz.Length) :: rest) alternate
        best 0 [("",doc)] false 

    /// Pretty print the document to a string.
    /// Lines are terminated with the operating systems default line terminator.
    let prettyPrint (doc:Doc) (width:int) : string = 
        let sb = StringBuilder ()
        let inline stringAppend (s:string) : unit = sb.Append(s) |> ignore
        let simpleDoc = layout width doc
        let rec work (sdoc:SimpleDoc) (cont:unit -> unit) : unit = 
            match sdoc with
            | SEmpty -> cont ()
            | SText(t,rest) -> 
                stringAppend t
                work rest cont
            | SLine(x,rest) -> 
                sb.AppendLine() |> ignore
                stringAppend x
                work rest cont

        work simpleDoc (fun _ -> ())
        sb.ToString()

    /// Render the document to a string.
    /// This is `prettyPrint` with arg order reversed
    let render (width:int) (doc:Doc) : string = prettyPrint doc width

    /// Output a document to file.
    /// Lines are terminated with the default line terminator.
    let writeDoc (width:int) (fileName:string) (doc:Doc) : unit = 
        use sw = IO.File.CreateText(fileName)
        let rec work (sdoc:SimpleDoc) (cont:unit -> unit) : unit = 
            match sdoc with
            | SEmpty -> cont ()
            | SText(t,rest) -> 
                sw.Write(t)     |> ignore
                work rest cont
            | SLine(x,rest) -> 
                sw.WriteLine()  |> ignore
                sw.Write(x)     |> ignore 
                work rest cont

        work (layout width doc) (fun _ -> ())

    
    /// Output a document to stdout.
    let printDoc (width:int) (doc:Doc) : unit = 
        let rec work (sdoc:SimpleDoc) (cont:unit -> unit) : unit = 
            match sdoc with
            | SEmpty -> cont ()
            | SText(t,rest) -> 
                printf "%s" t   |> ignore
                work rest cont
            | SLine(x,rest) -> 
                printfn ""      |> ignore
                printf "%s" x   |> ignore 
                work rest cont
        work (layout width doc) (fun _ -> printfn "")



    // ************************************************************************
    // Primitive printers   


    /// The empty document
    let empty : Doc = Nil
    
    /// 'nest' renders the document 'doc' with the current indentation level 
    /// increased by i
    let nest (i : int) (doc : Doc) : Doc = Nest (i, doc)
    
    /// Generate the document containing the literal string 's'.
    /// The input text should not contain newline characters.
    let text (s:string) : Doc = Text s

    /// Undocumented - used by align.
    /// 'column' gives access to the current column position.
    let column (f:int -> Doc) : Doc = Column(f)

    /// Undocumented - used by align.
    /// 'nesting' gives access to the current nesting position.
    /// The nesting position is not the same as the column position.
    let nesting (f:int -> Doc) : Doc = Nesting(f)

    /// Use the group combinator to specify alternate layouts.
    /// `(group doc)` undoes all linebreaks in doc.
    let group (doc:Doc) : Doc = Group(doc)

    /// 'line' advances to the next line and indents to the current nesting 
    /// level.
    /// If the line break is undone by group line is rendered as a space.
    let line : Doc = Line false

    /// 'linebreak' advances to the next line and indents to the current nesting 
    /// level.
    /// If the line break is undone by group line is rendered as empty.    
    let linebreak : Doc = Line true

    /// This is 'char' in PPrint (Haskell).
    let character (ch:char) : Doc = 
        match ch with
        | '\n' | '\r' -> line 
        | _ -> text <| ch.ToString()

    /// `softline` behaves like `space` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    let softline : Doc = group line

    /// `softbreak` behaves like `empty` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    let softbreak : Doc = group linebreak

    
    

    
    /// Concatenate documents x and y.
    let beside (x:Doc) (y:Doc) : Doc = Cat(x,y)

    // Don't try to define (<>) - it is a reserved operator name in F#


    /// Concatenate two documents horizontally (no separating space).
    /// This is (<>) in PPrint (Haskell).
    let ( ^^ ) (x:Doc) (y:Doc) = beside x y

    /// Concatenate two documents horizontally with a separating space.
    let besideSpace (x:Doc) (y:Doc) : Doc = x ^^ character ' ' ^^ y


    /// Concatenate two documents horizontally with a separating space.
    /// This is (<+>) in PPrint (Haskell).
    let ( ^+^ ) (x:Doc) (y:Doc) : Doc = besideSpace x y

    /// Concatenate two documents with a soft line.
    /// This is (</>) in PPrint (Haskell).
    let ( ^/^ ) (x:Doc) (y:Doc) : Doc = x ^^ softline ^^ y
    
    /// Concatenate two documents with a soft break.
    /// This is (<//>) in PPrint (Haskell).
    let ( ^//^ ) (x:Doc) (y:Doc) : Doc = x ^^ softbreak ^^ y

    /// Concatenate two documents separating with `line`.
    /// This is (<$>) in PPrint (Haskell).
    let ( ^!^ ) (x:Doc) (y:Doc) : Doc = x ^^ line ^^ y

    /// Concatenate two documents separating with `linebreak`.
    /// This is (<$$>) in PPrint (Haskell).
    let ( ^!!^ ) (x:Doc) (y:Doc) : Doc = x ^^ linebreak ^^ y


    
    
    // ************************************************************************
    // Character printers

    /// Single left parenthesis: '('
    let lparen : Doc = character '('

    /// Single right parenthesis: ')'
    let rparen : Doc = character ')'

    /// Single left angle: '<'
    let langle : Doc = character '<'

    /// Single right angle: '>'
    let rangle : Doc = character '>'

    /// Single left brace: '{'
    let lbrace : Doc = character '{'
    
    /// Single right brace: '}'
    let rbrace : Doc= character '}'
    
    /// Single left square bracket: '['
    let lbracket : Doc = character '['
    
    /// Single right square bracket: ']'
    let rbracket : Doc = character ']'


    /// Single quote: '
    let squote : Doc= character '\''

    ///The document @dquote@ contains a double quote, '\"'.
    let dquote : Doc = character '"'

    /// The document @semi@ contains a semi colon, \";\".
    let semi : Doc = character ';'

    /// The document @colon@ contains a colon, \":\".
    let colon : Doc = character ':'

    /// The document @comma@ contains a comma, \",\".
    let comma : Doc = character ','

    /// The document @space@ contains a single space, \" \".
    let space : Doc = character ' '

    /// The document @dot@ contains a single dot, \".\".
    let dot : Doc = character '.'

    /// The document @backslash@ contains a back slash, \"\\\".
    let backslash : Doc = character '\\'

    /// The document @equals@ contains an equal sign, \"=\".
    let equals : Doc = character '='

    /// Generate a document of n spaces.
    let spaces (n:int) : Doc = text <| String.replicate n " "

    /// Enclose the document body betwen l (left) and r (right).
    let enclose (l:Doc) (r:Doc) (body:Doc)   = l ^^ body ^^ r

    /// Enclose in signle quotes '...'
    let squotes (x:Doc) : Doc = enclose squote squote x
    
    /// Enclose in double quotes "..."
    let dquotes (x:Doc) : Doc = enclose dquote dquote x
    
    /// Enclose in angle braces {...}
    let braces (x:Doc) : Doc = enclose lbrace rbrace x
    
    /// Enclose in square brackets (...)
    let parens (x:Doc) : Doc = enclose lparen rparen x
    
    /// Enclose in angle brackets <...>
    let angles (x:Doc) : Doc = enclose langle rangle x
    
    /// Enclose in square brackets [...]
    let brackets (x:Doc) : Doc = enclose lbracket rbracket x

    // ************************************************************************
    // List concatenation 

    let foldDocs (op:Doc -> Doc -> Doc) (documents:Doc list) : Doc = 
        let rec work (acc:Doc) (ls:Doc list) (cont:Doc -> Doc) : Doc = 
            match ls with
            | [] -> cont acc
            | x :: xs -> work (op acc x) xs cont        
        match documents with
        | [] -> empty
        | (x::xs) -> work x xs id

    let punctuate (separator:Doc) (documents:Doc list) : Doc =
        foldDocs (fun l r -> l ^^ separator ^^ r) documents
    
    /// Separate documents horizontally with a space.
    let hsep (documents: Doc list) = foldDocs (^+^) documents

    /// Separate documents with (^!^)
    let vsep (documents: Doc list) = foldDocs (^!^) documents
    
    /// Separate documents with (^/^)
    let fillSep (documents: Doc list)  = foldDocs (^/^) documents

    let sep (documents: Doc list) = group (vsep documents)

    let hcat (documents: Doc list) = foldDocs (^^) documents

    /// Separate documents with (^!!^)
    let vcat (documents: Doc list) = foldDocs (^!!^) documents

    /// Separate documents with (^//^)
    let fillCat (documents: Doc list)  = foldDocs (^//^) documents


    /// Concat the list of docs with (^^) if the result fits the within a line,
    /// otherwise concat vertically with (^!!^).
    let cat (documents: Doc list) = group (vcat documents)

    /// Concatenante all documents with `separator` and bookend them 
    /// with `left` and `right`.
    let encloseSep (left:Doc) (right:Doc) (separator:Doc) (documents:Doc list) : Doc = 
        let rec work (acc:Doc) (docs:Doc list) (cont:Doc -> Doc) = 
            match docs with
            | [] -> cont acc
            | [x] -> cont (acc ^//^ x)
            | x :: xs -> 
                work (acc ^//^ x ^^ separator) xs cont
        work left documents (fun d -> d ^^ right)

    /// Enclose in square brackets and separate with comma [a,b,c,...]
    let commaList (docs:Doc list) : Doc = encloseSep lbracket rbracket comma docs

    /// Enclose in square brackets and separate with semicolon [a;b;c,...]
    let semiList (docs:Doc list) : Doc = encloseSep lbracket rbracket semi docs

    /// Enclose in parens and separate with comma (a,b,c,...)
    let tupled (docs:Doc list) : Doc = encloseSep lparen rparen comma docs

    /// Enclose in curly braces and separate with comma {a,b,c,...}
    let commaBraces  (docs:Doc list) : Doc = encloseSep lbrace rbrace comma docs

    /// Enclose in curly braces and separate with semicolon {a;b;c;...}
    let semiBraces  (docs:Doc list) : Doc = encloseSep lbrace rbrace semi docs

    let hcatSpace (docs:Doc list) : Doc = punctuate space docs


    let vcatSoft (docs:Doc list) : Doc = punctuate softline docs

    let vcatSoftBreak (docs:Doc list) : Doc = punctuate softbreak docs

    /// Undocumented.
    let width (doc:Doc) (fn:int -> Doc) : Doc = 
        column (fun k1 -> doc ^^ column (fun k2 -> fn (k2 - k1)) )

    /// `(align d)` renders the document `d` with the nesting level set to 
    /// the current column.
    let align (doc:Doc) :Doc = 
        column (fun k -> nesting (fun i -> nest (k - i) doc))

    /// Implement hanging indentation.
    let hang (i:int) (doc:Doc) : Doc = align (nest i doc)

    /// Indent the document `doc` with `i` spaces.
    let indent (i:int) (doc:Doc) : Doc = 
        hang i (spaces i ^^ doc)

    /// `fill` renders the supplied  document and right pads with spaces 
    /// until the width is equal to `i`.
    let fill (i:int) (doc:Doc) : Doc = 
        width doc (fun w -> if w >= i then empty else spaces (i - w))

    let fillBreak (f:int) (doc:Doc) : Doc = 
        width doc (fun w -> if w > f then nest f linebreak else spaces (f - w))



    /// Alias for `empty` (potentially avoids name clashes in user code).
    let emptyDoc : Doc = empty

    /// Print an int literal.
    let intDoc (i:int) : Doc = i.ToString() |> text
    
    /// Print a float literal.
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let floatDoc (d:float) : Doc = d.ToString() |> text
    
    /// Print a double literal.
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let doubleDoc (d:double) : Doc = d.ToString() |> text

    /// Print a single (float32) literal.
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let singleDoc (d:single) : Doc = d.ToString() |> text

    /// Print a decimal literal.
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let decimalDoc (d:decimal) : Doc = d.ToString() |> text

    /// Prints "true" or "false" (lowercase, F# style)
    let boolDoc (b:bool) : Doc = 
        (if b then "true" else "false") |> text

    /// Use this rather than text if the input string contains newlines.
    /// Newline characters are replaced by 'line'
    /// This is 'string' in PPrint (Haskell).
    let stringDoc (s:string) : Doc = 
        let lines = List.map text << Array.toList <| s.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
        punctuate line lines

    /// Print a unsigned byte literal as a decimal.
    /// Note no F# type specifying suffix is printed, if you want this
    /// functionality you need to write your own function.
    let byteDoc (i:byte) : Doc = 
        i.ToString() |> text
        
    /// Print a signed byte literal as a decimal.
    let sbyteDoc (i:sbyte) : Doc = 
        i.ToString() |> text

    /// Print a 16-bit signed byte literal as a decimal.
    let int16Doc (i:int16) : Doc = 
        i.ToString() |> text

    /// Print a 16-bit unsigned byte literal as a decimal.
    let uint16Doc (i:uint16) : Doc = 
        i.ToString() |> text

    /// Print a 32-bit signed byte literal as a decimal.
    let int32Doc (i:int32) : Doc = 
        i.ToString() |> text

    /// Print a 32-bit unsigned byte literal as a decimal.
    let uint32Doc (i:uint32) : Doc = 
        i.ToString() |> text

    /// Print a 64-bit signed byte literal as a decimal.        
    let int64Doc (i:int64) : Doc = 
        i.ToString() |> text

    /// Print a 64-bit unsigned byte literal as a decimal.
    let uint64Doc (i:uint64) : Doc = 
        i.ToString() |> text
    
    /// Print a 32-bit IEEE float. 
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let float32Doc (d:float32) : Doc = 
        d.ToString() |> text

    /// Print the value with the supplied format
    let inline formatted (fmt:Printf.StringFormat<'a->string,string>) (value:'a) : Doc = 
        sprintf fmt value |> text

