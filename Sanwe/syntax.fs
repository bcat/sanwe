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

module Sanwe.Syntax

open Sanwe.Common
open Sanwe.Types

type private LexChar =
    { ch      : char option
      rawCh   : char option
      pos     : Position
      pos'    : Position
      source' : LazyList<char>
      next    : Lazy<LexChar> }

type private LexState =
    { f       : LexState -> LexChar -> LexState
      o       : Token option
      pos     : Position
      pos'    : Position
      source' : LazyList<char> }

let lex s pos source =
    let (|Layout|_|) = function
        | Some ('\t' | '\n' | ' ') as x -> x
        | _ -> None
    
    let (|Digit|_|) = function
        | Some ch when '0' <= ch && ch <= '9' -> Some ch
        | _ -> None
    
    let (|BinDigit|_|) = function
        | Some ('0' | '1') as ch -> ch
        | _ -> None
    
    let (|OctDigit|_|) = function
        | Some ch when '0' <= ch && ch <= '7' -> Some ch
        | _ -> None
    
    let (|HexDigit|_|) = function
        | Some ch when '0' <= ch && ch <= '9' ||
                       'a' <= ch && ch <= 'f' ||
                       'A' <= ch && ch <= 'F' -> Some ch
        | _ -> None
    
    let (|Lower|_|) = function
        | Some ch when 'a' <= ch && ch <= 'z' -> Some ch
        | _ -> None
    
    let (|Upper|_|) = function
        | Some ch when 'A' <= ch && ch <= 'Z' -> Some ch
        | _ -> None
    
    let (|Alphanum|_|) = function
        | (Lower _ | Upper _ | Digit _ | Some '_') as ch -> ch
        | _ -> None
    
    let (|Graphic|_|) = function
        | Some ('#' | '$' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<'
                    | '=' | '>' | '?' | '@' | '^' | '~') as ch -> ch
        | _ -> None
    
    let (|Solo|_|) = function
        | Some ('!' | '%' | '(' | ')' | ',' | ';' | '[' | ']' | '{' | '|'
                    | '}') as ch -> ch
        | _ -> None
    
    let lexQuotedCh delim lc =
        let rec octEscape acc lc =
            match lc with
            | { rawCh = OctDigit ch; next = Lazy lc2 } ->
                octEscape (ch :: acc) lc2
            | { rawCh = Some '\\' } ->
                let ch = (acc |> List.rev |> Int32.of_chars 8 |> char)
                
                ch, lc.pos', lc.source'
            | _ ->
                raise (SyntaxError ("Unexpected character.", lc.pos, lc.pos',
                                    lc.source'))
        
        let rec hexEscape acc lc =
            match lc with
            | { rawCh = HexDigit ch; next = Lazy lc2 } ->
                hexEscape (ch :: acc) lc2
            | { rawCh = Some '\\' } ->
                let ch = (acc |> List.rev |> Int32.of_chars 16 |> char)
                
                ch, lc.pos', lc.source'
            | _ ->
                raise (SyntaxError ("Unexpected character.", lc.pos, lc.pos',
                                    lc.source'))
        
        match lc with
        | { rawCh = Some ch
            next  = Lazy ({ rawCh = Some ch2 } as lc2) }
          when ch = delim && ch2 = delim ->
            delim, lc2.pos', lc2.source'
        | { rawCh = Graphic ch | Alphanum ch | Solo ch | Some (' ' as ch) } ->
            ch, lc.pos', lc.source'
        | { rawCh = Some '\\'; next = Lazy lc2 } ->
            match lc2 with
            | { rawCh = Some ('"' | '\'' | '`' as ch2) } ->
                ch2, lc2.pos', lc2.source'
            | { rawCh = Some 'a' } -> '\u0007', lc2.pos', lc2.source'
            | { rawCh = Some 'b' } -> '\b', lc2.pos', lc2.source'
            | { rawCh = Some 'f' } -> '\u000C', lc2.pos', lc2.source'
            | { rawCh = Some 'n' } -> '\n', lc2.pos', lc2.source'
            | { rawCh = Some 'r' } -> '\r', lc2.pos', lc2.source'
            | { rawCh = Some 't' } -> '\t', lc2.pos', lc2.source'
            | { rawCh = Some 'v' } -> '\u000B', lc2.pos', lc2.source'
            | { rawCh = OctDigit ch2; next = Lazy lc3 } ->
                octEscape [ ch2 ] lc3
            | { rawCh = Some 'x'
                next = Lazy { rawCh = HexDigit ch3; next = Lazy lc4 } } ->
                hexEscape [ ch3 ] lc4
            | { rawCh = None } ->
                raise (SyntaxError ("Unexpected end-of-file.", lc.pos, lc.pos',
                                    lc.source'))
            | _ ->
                raise (SyntaxError ("Unexpected character.", lc.pos, lc.pos',
                                    lc.source'))
        | { rawCh = None } ->
            raise (SyntaxError ("Unexpected end-of-file.", lc.pos, lc.pos',
                                lc.source'))
        | _ -> raise (SyntaxError ("Unexpected character.", lc.pos, lc.pos',
                                   lc.source'))
    
    let rec start ls lc =
        match lc with
        | { ch = None } -> { ls with o = Some EOF; pos = lc.pos }
        | { ch = Layout _ } -> { ls with f = layout }
        | { ch = Some '%' } -> { ls with f = lineComment }
        | { ch = Some '/'; next = Lazy lc2 } ->
            match lc2 with
            | { ch = Some '*' } ->
                { ls with f       = blockComment
                        ; pos'    = lc2.pos'
                        ; source' = lc2.source' }
            | _ -> graphic [ '/' ] ls lc
        | { ch = Some '.'; next = Lazy lc2 } ->
            match lc2 with
            | { ch = Layout _ | Some '%' } ->
                { ls with o = Some End; pos = lc.pos }
            | _ -> graphic [ '.' ] ls lc
        | { ch = Some '!' } -> { ls with o = Some (Name "!"); pos = lc.pos }
        | { ch = Some '[' } -> { ls with o = Some OpenList; pos = lc.pos }
        | { ch = Some '{' } -> { ls with o = Some OpenCurly; pos = lc.pos }
        | { ch = Some '(' } -> { ls with o = Some OpenCT; pos = lc.pos }
        | { ch = Some ')' } -> { ls with o = Some Close; pos = lc.pos }
        | { ch = Some ',' } -> { ls with o = Some Comma; pos = lc.pos }
        | { ch = Some ';' } -> { ls with o = Some (Name ";"); pos = lc.pos }
        | { ch = Some ']' } -> { ls with o = Some CloseList; pos = lc.pos } 
        | { ch = Some '|' } -> { ls with o = Some HTSeparator; pos = lc.pos }
        | { ch = Some '}' } -> { ls with o = Some CloseCurly; pos = lc.pos }
        | { ch = Some '0'; next = Lazy ({ next = Lazy lc3 } as lc2) } ->
            match lc2, lc3 with
            | { ch = Some '\'' }, _ ->
                match lc3 with
                | { rawCh = Some '\\'; next = Lazy lc4 } ->
                    match lc4 with
                    | { rawCh = Some '\n' } ->
                        { ls with o = Some (IntegerLit 0); pos = lc.pos }
                    | _ -> failwith "Not implemented."
                | _ ->
                    let ch, pos', source' = lexQuotedCh '\'' lc3
                    
                    { ls with o       = Some (ch |> int |> IntegerLit)
                            ; pos     = lc.pos
                            ; pos'    = pos'
                            ; source' = source' }
            | { ch = Some 'b' }, { ch = BinDigit ch3 } ->
                binInteger [ ch3 ] { ls with pos     = lc.pos
                                           ; pos'    = lc3.pos'
                                           ; source' = lc3.source' } lc3
            | { ch = Some 'o' }, { ch = OctDigit ch3 } ->
                octInteger [ ch3 ] { ls with pos     = lc.pos
                                           ; pos'    = lc3.pos'
                                           ; source' = lc3.source' } lc3
            | { ch = Some 'x' }, { ch = HexDigit ch3 } ->
                hexInteger [ ch3 ] { ls with pos     = lc.pos
                                           ; pos'    = lc3.pos'
                                           ; source' = lc3.source' } lc3
            | _ -> integer [ '0' ] { ls with pos = lc.pos } lc
        | { ch = Some '"' } ->
            { ls with f = quote DoubleQuotedList '"' []; pos = lc.pos }
        | { ch = Some '\'' } ->
            { ls with f   = quote (String.of_chars >> Name) '\'' []
                    ; pos = lc.pos }
        | { ch = Some '`' } ->
            { ls with f   = quote (String.of_chars >> BackQuotedString) '`' []
                    ; pos = lc.pos }
        | { ch = Digit ch } -> integer [ ch ] { ls with pos = lc.pos } lc
        | { ch = Graphic ch } -> graphic [ ch ] { ls with pos = lc.pos } lc
        | { ch = Upper ch | Some ('_' as ch) } ->
            ident VariableName [ ch ] { ls with pos = lc.pos } lc
        | { ch = Lower ch } -> ident Name [ ch ] { ls with pos = lc.pos } lc
        | _ -> raise (SyntaxError ("Unexpected character.", lc.pos, lc.pos',
                                   lc.source'))
    
    and layout ls lc =
        match lc with
        | { ch = Some '(' } -> { ls with o = Some Open; pos = lc.pos }
        | _ -> start ls lc
    
    and lineComment ls lc =
        match lc with
        | { ch = None } -> { ls with o = Some EOF; pos = lc.pos }
        | { ch = Some '\n' } -> { ls with f = layout }
        | _ -> ls
    
    and blockComment ls lc =
        match lc with
        | { ch = None } ->
            raise (SyntaxError ("Unexpected end-of-file.", lc.pos, lc.pos',
                                lc.source'))
        | { ch = Some '*'; next = Lazy lc2 } ->
            match lc2 with
            | { ch = Some '/' } ->
                { ls with f = layout; pos' = lc2.pos'; source' = lc2.source' }
            | _ -> ls
        | _ -> ls
    
    and binInteger acc ls { next = Lazy lc2 } =
        match lc2 with
        | { ch = BinDigit ch2 } -> { ls with f = binInteger (ch2 :: acc) }
        | _ ->
            { ls with o = Some (acc
                                |> List.rev
                                |> Int32.of_chars 2
                                |> IntegerLit) }
    
    and octInteger acc ls { next = Lazy lc2 } =
        match lc2 with
        | { ch = OctDigit ch2 } -> { ls with f = octInteger (ch2 :: acc) }
        | _ ->
            { ls with o = Some (acc
                                |> List.rev
                                |> Int32.of_chars 8
                                |> IntegerLit) }
    
    and hexInteger acc ls { next = Lazy lc2 } =
        match lc2 with
        | { ch = HexDigit ch2 } -> { ls with f = hexInteger (ch2 :: acc) }
        | _ ->
            { ls with o = Some (acc
                                |> List.rev
                                |> Int32.of_chars 16
                                |> IntegerLit) }
    
    and quote cons delim acc ls lc =
        match lc with
        | { rawCh = Some '\\'; next = Lazy lc2 } ->
            match lc2 with
            | { rawCh = Some '\n' } ->
                { ls with f       = quote cons delim acc
                        ; pos'    = lc2.pos'
                        ; source' = lc2.source' }
            | _ ->
                let ch, pos', source' = lexQuotedCh '\'' lc
                
                { ls with f       = quote cons delim (ch :: acc)
                        ; pos'    = pos'
                        ; source' = source' }
        | { rawCh = Some ch; next = Lazy lc2 } when ch = delim ->
            match lc2 with
            | { rawCh = Some ch2 } when ch2 = delim ->
                let ch, pos', source' = lexQuotedCh '\'' lc
                
                { ls with f       = quote cons delim (ch :: acc)
                        ; pos'    = pos'
                        ; source' = source' }
            | _ ->
                { ls with o = Some (acc |> List.rev |> cons) }
        | _ ->
            let ch, pos', source' = lexQuotedCh '\'' lc
            
            { ls with f       = quote cons delim (ch :: acc)
                    ; pos'    = pos'
                    ; source' = source' }
    
    and integer acc ls { next = Lazy lc2 } =
        match lc2 with
        | { ch = Digit ch2 } -> { ls with f = integer (ch2 :: acc) }
        | { ch = Some '.'; next = Lazy lc3 } ->
            match lc3 with
            | { ch = Digit ch3 } ->
                { ls with f       = fraction (ch3 :: '.' :: acc)
                        ; pos'    = lc2.pos'
                        ; source' = lc2.source' }
            | _ ->
                { ls with o = Some (acc
                                    |> List.rev
                                    |> Int32.of_chars 10
                                    |> IntegerLit) }
        | _ ->
            { ls with o = Some (acc
                                |> List.rev
                                |> Int32.of_chars 10
                                |> IntegerLit) }
    
    and fraction acc ls { next = Lazy lc2 } =
        match lc2 with
        | { ch = Digit ch2 } -> { ls with f = fraction (ch2 :: acc) }
        | { ch = Some ('E' | 'e' as ch2); next = Lazy lc3 } ->
            match lc3 with
            | { ch = Digit ch3 } ->
                { ls with f       = exponent (ch3 :: ch2 :: acc)
                        ; pos'    = lc2.pos'
                        ; source' = lc2.source' }
            | { ch = Some ('+' | '-' as ch3)
                next = Lazy { ch = Digit ch4 } } as lc3 ->
                { ls with f       = exponent (ch4 :: ch3 :: ch2 :: acc)
                        ; pos'    = lc3.pos'
                        ; source' = lc3.source' }
            | _ ->
                { ls with o = Some (acc
                                |> List.rev
                                |> Double.of_chars
                                |> FloatLit) }
        | _ ->
            { ls with o = Some (acc
                                |> List.rev
                                |> Double.of_chars
                                |> FloatLit) }
    
    and exponent acc ls { next = Lazy lc2 } =
        match lc2 with
        | { ch = Digit ch2 } -> { ls with f = exponent (ch2 :: acc) }
        | _ ->
            { ls with o = Some (acc
                                |> List.rev
                                |> Double.of_chars
                                |> FloatLit) }
    
    and graphic acc ls { next = Lazy lc2 } =
        match lc2 with
        | { ch = Graphic ch2 } -> { ls with f = graphic (ch2 :: acc) }
        | _ ->
            { ls with o = Some (acc |> List.rev |> String.of_chars |> Name) }
    
    and ident cons acc ls { next = Lazy lc2 } =
        match lc2 with
        | { ch = Alphanum ch2 } -> { ls with f = ident cons (ch2 :: acc) }
        | _ ->
            { ls with o = Some (acc |> List.rev |> String.of_chars |> cons) }
    
    let rec lex' ls =
        let rec getLc pos source =
            let ch, rawCh, pos', source' =
                match source with
                | LazyList.Nil -> None, None, pos, source
                | LazyList.Cons (ch, source') ->
                    let rawCh = Some ch
                    
                    let ch =
                        if s.convFlag then
                            match Map.tryFind ch s.convMap with
                            | Some ch -> ch
                            | None -> ch
                        else
                            ch
                    
                    let pos' =
                        match ch with
                        | '\n' -> Position.nextLn pos
                        | _ -> Position.next pos
                    
                    Some ch, rawCh, pos', source'
            
            { ch      = ch
              rawCh   = rawCh
              pos     = pos
              pos'    = pos'
              source' = source'
              next    = lazy (getLc pos' source') }
        
        let lc = getLc ls.pos' ls.source'
        
        let ls' = ls.f { ls with pos' = lc.pos'; source' = lc.source' } lc
        
        match ls'.o with
        | None -> lex' ls'
        | Some token -> token, ls'.pos, ls'.pos', ls'.source'
    
    lex' { f       = start
           o       = None
           pos     = pos
           pos'    = pos
           source' = source }

let parse s pos =
    let (|LeftOperator|_|) name =
        match Map.tryFind name s.leftOperators with
        | Some (priority, specifier) ->
            Some (priority, specifier)
        | None -> None
    
    let (|RightOperator|_|) name =
        match name with
        | "," -> Some (1000, XFY)
        | _ ->
            match Map.tryFind name s.rightOperators with
            | Some (priority, specifier) ->
                Some (priority, specifier)
            | None -> None
    
    let (|Delim|_|) token =
        match token with
        | Close | CloseCurly | CloseList | Comma | End
        | HTSeparator as delim ->
            Some delim
        | _ -> None
    
    let rec panic msg token pos pos' source' =
        match token with
        | EOF | End -> raise (SyntaxError (msg, pos, pos', source'))
        | _ ->
            let token, _, pos', source' = lex s pos' source'
            
            panic msg token pos pos' source'
    
    let parseDoubleQuotedList chs =
        match s.doubleQuotesFlag with
        | DoubleQuotesAtom -> (chs |> String.of_chars |> Atom)
        | DoubleQuotesChars ->
            List.foldBack (string >> Atom >> Term.cons) chs Term.nil
        | DoubleQuotesCodes ->
            List.foldBack (int >> Integer >> Term.cons) chs Term.nil
    
    let rec parseTerm maxPriority pos source =
        let parseLeft pos source =
            let parseParen pos source =
                let term, pos', source' = parseTerm 1201 pos source
                
                match lex s pos' source' with
                | Close, _, pos', source' -> term, 0, pos', source'
                | x -> x |> uncurry4 (panic "Unmatched opening parenthesis.")
            
            let parseCurly pos source =
                match lex s pos source with
                | CloseCurly, _, pos', source' -> Atom "{}", 0, pos', source'
                | _ ->
                    let term, pos', source' = parseTerm 1201 pos source
                    
                    match lex s pos' source' with
                    | CloseCurly, _, pos', source' ->
                        Compound ("{}", [ term ]), 0, pos', source'
                    | x -> x |> uncurry4 (panic "Unmatched opening curly.")
            
            let parseArg pos source =
                match lex s pos source with
                | Name name, _, pos', source' ->
                    match lex s pos' source' with
                    | Delim delim, _, _, _ -> Atom name, pos', source'
                    | _ -> parseTerm 999 pos source
                | _ -> parseTerm 999 pos source
            
            let parseList pos source =
                let rec parseTail pos source =
                    match lex s pos source with
                    | CloseList, _, pos', source' -> Term.nil, pos', source'
                    | HTSeparator, _, pos', source' ->
                        let tail, pos', source' = parseArg pos' source'
                        
                        match lex s pos' source' with
                        | CloseList, _, pos', source' -> tail, pos', source'
                        | x -> x |> uncurry4 (panic "Illegal list.")
                    | Comma, _, pos', source' ->
                        let head, pos', source' = parseArg pos source
                        let tail, pos', source' = parseTail pos' source'
                        
                        Term.cons head tail, pos', source'
                    | x -> x |> uncurry4 (panic "Illegal list.")
                
                match lex s pos source with
                | CloseList, _, pos', source' -> Term.nil, 0, pos', source'
                | _ ->
                    let head, pos', source' = parseArg pos source
                    let tail, pos', source' = parseTail pos' source'
                    
                    Term.cons head tail, 0, pos', source'
            
            let parseName name pos source =
                let parseCompound name pos source =
                    let rec parseArgList acc pos source =
                        let arg, pos', source' = parseArg pos source
                        
                        match lex s pos' source' with
                        | Close, _, pos', source' ->
                            List.rev (arg :: acc), pos', source'
                        | Comma, _, pos', source' ->
                            parseArgList (arg :: acc) pos' source'
                        | x -> x |> uncurry4 (panic ("Expected comma or " +
                                                     "close token."))
                    
                    let argList, pos', source' = parseArgList [] pos source
                    
                    Compound (name, argList), 0, pos', source'
                
                match name, lex s pos source with
                | _, (OpenCT, _, pos', source') ->
                    parseCompound name pos' source'
                | "-", (IntegerLit value, _, pos', source') ->
                    Integer -value, 0, pos', source'
                | "-", (FloatLit value, _, pos', source') ->
                    Float -value, 0, pos', source'
                | LeftOperator _, (Delim _, _, _, _) ->
                    Atom name, 1201, pos, source
                | LeftOperator (priority, FX), _
                  when priority <= maxPriority ->
                    let right, pos', source' =
                        parseTerm (priority - 1) pos source
                    
                    Compound (name, [ right ]), priority, pos', source'
                | LeftOperator (priority, FY), _
                  when priority <= maxPriority ->
                    let right, pos', source' = parseTerm priority pos source
                    
                    Compound (name, [ right ]), priority, pos', source'
                | (LeftOperator _ | RightOperator _), _ ->
                    Atom name, 1201, pos, source
                | _ -> Atom name, 0, pos, source
            
            match lex s pos source with
            | Name name, _, pos', source' as x ->
                match parseName name pos' source' with
                | _, priority, _, _ when priority > maxPriority ->
                    x |> uncurry4 (panic "Expression must be parenthesized.")
                | x -> x
            | DoubleQuotedList chs, _, pos', source' ->
                match parseDoubleQuotedList chs with
                | Atom name -> parseName name pos' source'
                | term -> term, 0, pos', source'
            | (Open | OpenCT), _, pos', source' -> parseParen pos' source'
            | OpenCurly, _, pos', source' -> parseCurly pos' source'
            | OpenList, _, pos', source' -> parseList pos' source'
            | IntegerLit value, _, pos', source' ->
                Integer value, 0, pos', source'
            | FloatLit value, _, pos', source' -> Float value, 0, pos', source'
            | VariableName name, _, pos', source' ->
                Variable name, 0, pos', source'
            | x -> x |> uncurry4 (panic "Expected term.")
        
        let rec parseRight left leftPriority pos source =
            let parseName name pos source pos' source' =
                match name with
                | RightOperator (priority, XF)
                  when priority <= maxPriority && leftPriority < priority ->
                    parseRight (Compound (name, [ left ])) priority pos' source'
                | RightOperator (priority, YF)
                  when priority <= maxPriority && leftPriority <= priority ->
                    parseRight (Compound (name, [ left ])) priority pos' source'
                | RightOperator (priority, XFX)
                  when priority <= maxPriority && leftPriority < priority ->
                    let right, pos', source' =
                        parseTerm (priority - 1) pos' source'
                    
                    parseRight (Compound (name, [ left; right ])) priority pos'
                        source'
                | RightOperator (priority, YFX)
                  when priority <= maxPriority && leftPriority <= priority ->
                    let right, pos', source' =
                        parseTerm (priority - 1) pos' source'
                    
                    parseRight (Compound (name, [ left; right ])) priority pos'
                        source'
                | RightOperator (priority, XFY)
                  when priority <= maxPriority && leftPriority < priority ->
                    let right, pos', source' = parseTerm priority pos' source'
                    
                    parseRight (Compound (name, [ left; right ])) priority pos'
                        source'
                | _ -> left, pos, source
            
            match lex s pos source with
            | Name name, _, pos', source' ->
                parseName name pos source pos' source'
            | Comma, _, pos', source' -> parseName "," pos source pos' source'
            | DoubleQuotedList chs, _, pos', source' ->
                match parseDoubleQuotedList chs with
                | Atom name -> parseName name pos source pos' source'
                | _ -> left, pos, source
            | _ -> left, pos, source
        
        (pos, source) |> uncurry parseLeft |> uncurry4 parseRight
    
    function
    | LazyList.Nil -> None
    | source ->
        match lex s pos source with
        | EOF, _, _, _ -> None
        | _ ->
            let term, pos', source' = parseTerm 1201 pos source
            
            match lex s pos' source' with
            | End, _, pos', source' -> Some (term, pos', source')
            | x -> x |> uncurry4 (panic "Expected end token.")
