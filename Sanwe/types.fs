(* Copyright © 2009 Jonathan Rascher.

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module Sanwe.Types

type Term =
    | Atom of string
    | Compound of string * Term list
    | Float of float
    | Integer of int
    | Variable of string

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Term =
    let nil = Atom "[]"
    
    let cons x xs = Compound (".", [ x; xs ])

type DoubleQuotesFlag =
    | DoubleQuotesAtom
    | DoubleQuotesChars
    | DoubleQuotesCodes

type UnknownFlag =
    | UnknownError
    | UnknownFail
    | UnknownWarning

type LeftSpecifier =
    | FX
    | FY

type RightSpecifier =
    | XFX
    | YFX
    | XFY
    | XF
    | YF

type TerpState =
    { db               : Map<string * int, Term>
      convFlag         : bool
      debugFlag        : bool
      doubleQuotesFlag : DoubleQuotesFlag
      unknownFlag      : UnknownFlag
      convMap          : Map<char, char>
      leftOperators    : Map<string, int * LeftSpecifier>
      rightOperators   : Map<string, int * RightSpecifier> }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module TerpState =
    let initial =
        { db               = Map.empty
          convFlag         = true
          debugFlag        = false
          doubleQuotesFlag = DoubleQuotesCodes
          unknownFlag      = UnknownError
          convMap          = Map.empty
          leftOperators    = Map.of_list <|
                             [ ":-",   (1200, FX)
                               "?-",   (1200, FX)
                               "\+",   (900,  FY)
                               "-",    (200,  FY)
                               "\\",   (200,  FY) ]
          rightOperators   = Map.of_list <|
                             [ ":-",   (1200, XFX)
                               "-->",  (1200, XFX)
                               ";",    (1100, XFY)
                               "->",   (1050, XFY)
                               "=",    (700,  XFX)
                               "\\=",  (700,  XFX)
                               "==",   (700,  XFX)
                               "\\==", (700,  XFX)
                               "@<",   (700,  XFX)
                               "@=<",  (700,  XFX)
                               "@>",   (700,  XFX)
                               "@>=",  (700,  XFX)
                               "is",   (700,  XFX)
                               "=:=",  (700,  XFX)
                               "=\\=", (700,  XFX)
                               "<",    (700,  XFX)
                               "=<",   (700,  XFX)
                               ">",    (700,  XFX)
                               ">=",   (700,  XFX)
                               "=..",  (700,  XFX)
                               "+",    (500,  YFX)
                               "-",    (500,  YFX)
                               "/\\",  (500,  YFX)
                               "\\/",  (500,  YFX)
                               "*",    (400,  YFX)
                               "/",    (400,  YFX)
                               "//",   (400,  YFX)
                               "rem",  (400,  YFX)
                               "mod",  (400,  YFX)
                               "<<",   (400,  YFX)
                               ">>",   (400,  YFX)
                               "**",   (200,  XFX)
                               "^",    (200,  XFY) ] }

type Token =
    | BackQuotedString of string
    | Close
    | CloseCurly
    | CloseList
    | Comma
    | DoubleQuotedList of char list
    | End
    | EOF
    | FloatLit of float
    | HTSeparator
    | IntegerLit of int
    | Name of string
    | Open
    | OpenCT
    | OpenCurly
    | OpenList
    | VariableName of string

type Position =
    { line : int
      col  : int }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Position =
    let next p = { p with col = p.col + 1 }
    
    let nextLn p = { p with line = p.line + 1; col = 1 }

exception SyntaxError of string * Position * Position * LazyList<char>
