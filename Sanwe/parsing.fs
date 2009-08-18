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

module Sanwe.Parsing

open Sanwe.Common
open Sanwe.Lexing

type term =
    | Atom of string
    | Compound of string * term list
    | Float of float
    | Integer of int
    | Variable of string

type doubleQuotesFlag =
    | DoubleQuotesAtom
    | DoubleQuotesChars
    | DoubleQuotesCodes

type leftSpecifier =
    | FX
    | FY

type rightSpecifier =
    | XFX
    | YFX
    | XFY
    | XF
    | YF

let defaultLeftOperators =
    Map.of_list <|
    [ ":-", (1200, FX);
      "?-", (1200, FX);
      "\+", (900,  FY);
      "-",  (200,  FY);
      "\\", (200,  FY); ]

let defaultRightOperators =
    Map.of_list <|
    [ ":-",   (1200, XFX);
      "-->",  (1200, XFX);
      ";",    (1100, XFY);
      "->",   (1050, XFY);
      "=",    (700,  XFX);
      "\\=",  (700,  XFX);
      "==",   (700,  XFX);
      "\\==", (700,  XFX);
      "@<",   (700,  XFX);
      "@=<",  (700,  XFX);
      "@>",   (700,  XFX);
      "@>=",  (700,  XFX);
      "is",   (700,  XFX);
      "=:=",  (700,  XFX);
      "=\\=", (700,  XFX);
      "<",    (700,  XFX);
      "=<",   (700,  XFX);
      ">",    (700,  XFX);
      ">=",   (700,  XFX);
      "=..",  (700,  XFX);
      "+",    (500,  YFX);
      "-",    (500,  YFX);
      "/\\",  (500,  YFX);
      "\\/",  (500,  YFX);
      "*",    (400,  YFX);
      "/",    (400,  YFX);
      "//",   (400,  YFX);
      "rem",  (400,  YFX);
      "mod",  (400,  YFX);
      "<<",   (400,  YFX);
      ">>",   (400,  YFX);
      "**",   (200,  XFX);
      "^",    (200,  XFY); ]

let rec parse doubleQuotesFlag leftOperators rightOperators tokens =
    let (|Token|EOF|) = LazyList.(|Cons|Nil|)
    
    let (|Delimiter|_|) tokens =
        match tokens with
        | Token ((Close | CloseCurly | CloseList | Comma | End |
                 HTSeparator as hd), tl) -> Some (hd, tl)
        | _ -> None
    
    let (|LeftOperator|_|) name =
        match Map.tryFind name leftOperators with
        | Some (priority, specifier) ->
            Some (priority, specifier)
        | None -> None
    
    let (|RightOperator|_|) name =
        match name with
        | "," -> Some (1000, XFY)
        | _ ->
            match Map.tryFind name rightOperators with
            | Some (priority, specifier) ->
                Some (priority, specifier)
            | None -> None
    
    let rec parseTerm maxPriority tokens =
        let parseLeft tokens =
            let parseArg tokens =
                match tokens with
                | Token (Name name, Delimiter (delimiter, tokens)) ->
                    Atom name, LazyList.cons delimiter tokens
                | _ -> parseTerm 999 tokens
            
            let parseCompound name tokens =
                let rec parseArgList acc tokens =
                    let arg, tokens = parseArg tokens
                    
                    match tokens with
                    | Token (Close, tokens) -> List.rev (arg :: acc), tokens
                    | Token (Comma, tokens) -> parseArgList (arg :: acc) tokens
                    | _ -> failwith "Syntax error."
                
                let argList, tokens = parseArgList [] tokens
                
                Compound (name, argList), 0, tokens
            
            let parseParen tokens =
                match parseTerm 1201 tokens with
                | term, Token (Close, tokens) -> term, 0, tokens
                | _ -> failwith "Syntax error."
            
            let parseCurly tokens =
                match tokens with
                | Token (CloseCurly, tokens) -> Atom "{}", 0, tokens
                | _ ->
                    match parseTerm 1201 tokens with
                    | term, Token (CloseCurly, tokens) ->
                        Compound ("{}", [term]), 0, tokens
                    | _ -> failwith "Syntax error."
            
            let parseList tokens =
                let rec parseTail tokens =
                    match tokens with
                    | Token (CloseList, tokens) -> Atom "[]", tokens
                    | Token (HTSeparator, tokens) ->
                        let tail, tokens = parseArg tokens
                        
                        match tokens with
                        | Token (CloseList, tokens) -> tail, tokens
                        | _ -> failwith "Syntax error."
                    | Token (Comma, tokens) ->
                        let head, tokens = parseArg tokens
                        let tail, tokens = parseTail tokens
                        
                        Compound (".", [head; tail]), tokens
                    | _ -> failwith "Syntax error."
                
                match tokens with
                | Token (CloseList, tokens) -> Atom "[]", 0, tokens
                | _ ->
                    let head, tokens = parseArg tokens
                    let tail, tokens = parseTail tokens
                    
                    Compound (".", [head; tail]), 0, tokens
            
            let parseName name tokens =
                match name, tokens with
                | _, Token (OpenCT, tokens) -> parseCompound name tokens
                | "-", Token (IntegerLit value, tokens) ->
                    Integer -value, 0, tokens
                | "-", Token (FloatLit value, tokens) -> Float -value, 0, tokens
                | RightOperator _, _ -> Atom name, 1201, tokens
                | LeftOperator _, Delimiter _ -> Atom name, 1201, tokens
                | LeftOperator (priority, FX), _
                  when priority <= maxPriority ->
                    let right, tokens = parseTerm (priority - 1) tokens
                    
                    Compound (name, [right]), priority, tokens
                | LeftOperator (priority, FY), _
                  when priority <= maxPriority ->
                    let right, tokens = parseTerm priority tokens
                    
                    Compound (name, [right]), priority, tokens
                | LeftOperator _, _ -> Atom name, 1201, tokens
                | _ -> Atom name, 0, tokens
            
            let parseDoubleQuotedList chars tokens =
                let term =
                    match doubleQuotesFlag with
                    | DoubleQuotesAtom ->
                        Atom (new string (Array.of_list chars))
                    | DoubleQuotesChars ->
                        List.foldBack
                            (fun ch tail ->
                                Compound (".", [Atom (string ch); tail]))
                            chars (Atom "[]")
                    | DoubleQuotesCodes ->
                        List.foldBack
                            (fun ch tail ->
                                Compound (".", [Integer (int ch); tail]))
                            chars (Atom "[]")
                
                term, 0, tokens
            
            match tokens with
            | Token (Name name, tokens) -> parseName name tokens
            | Token (DoubleQuotedList chars, tokens) ->
                parseDoubleQuotedList chars tokens
            | Token ((Open | OpenCT), tokens) -> parseParen tokens
            | Token (OpenCurly, tokens) -> parseCurly tokens
            | Token (OpenList, tokens) -> parseList tokens
            | Token (IntegerLit value, tokens) ->
                Integer value, 0, tokens
            | Token (FloatLit value, tokens) ->
                Float value, 0, tokens
            | Token ((VariableName name), tokens) ->
                Variable name, 0, tokens
            | _ -> failwith "Syntax error."
        
        let rec parseRight left leftPriority tokens =
            let parseName name token tokens =
                match name with
                | RightOperator (priority, XF)
                  when priority <= maxPriority && leftPriority < priority ->
                    parseRight (Compound (name, [left])) priority tokens
                | RightOperator (priority, YF)
                  when priority <= maxPriority && leftPriority <= priority ->
                    parseRight (Compound (name, [left])) priority tokens
                | RightOperator (priority, XFX)
                  when priority <= maxPriority && leftPriority < priority ->
                    let right, tokens = parseTerm (priority - 1) tokens
                    
                    parseRight (Compound (name, [left; right])) priority tokens
                | RightOperator (priority, YFX)
                  when priority <= maxPriority && leftPriority <= priority ->
                    let right, tokens = parseTerm (priority - 1) tokens
                    
                    parseRight (Compound (name, [left; right])) priority tokens
                | RightOperator (priority, XFY)
                  when priority <= maxPriority && leftPriority < priority ->
                    let right, tokens = parseTerm priority tokens
                    
                    parseRight (Compound (name, [left; right])) priority tokens
                | _ -> left, (LazyList.cons token tokens)
            
            match tokens with
            | Token (Name name as token, tokens) -> parseName name token tokens
            | Token (Comma as token, tokens) -> parseName "," token tokens
            | _ -> left, tokens
        
        tokens |> parseLeft |> uncurry3 parseRight
    
    match tokens with
    | EOF -> LazyList.empty ()
    | _ ->
        match parseTerm 1201 tokens with
        | term, Token (End, tokens) ->
            LazyList.consf term (fun () ->
                 parse doubleQuotesFlag leftOperators rightOperators tokens)
        | _ -> failwith "Syntax error."
