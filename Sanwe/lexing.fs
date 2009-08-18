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

module Sanwe.Lexing

open System.Text

open Sanwe.Common

type token =
    | BackQuotedString of string
    | Close
    | CloseCurly
    | CloseList
    | Comma
    | DoubleQuotedList of char list
    | End
    | FloatLit of float
    | HTSeparator
    | IntegerLit of int
    | Name of string
    | Open
    | OpenCT
    | OpenCurly
    | OpenList
    | VariableName of string

let rec lex xs =
    let eof = LazyList.empty
    
    let token token source = LazyList.consf token (fun () -> lex source)
    
    let (|Char|EOF|) = LazyList.(|Cons|Nil|)
    
    let (|EOL|_|) = function
        | Char ('\t', Char ('\n', xs)) | Char (('\r' | '\n'), xs) -> Some xs
        | _ -> None
    
    let (|Layout|_|) =
        let rec lineComment = function
            | EOF -> Some (eof ())
            | EOL xs -> Some xs
            | Char (_, xs) -> lineComment xs
        
        let rec blockComment = function
            | EOF -> failwith "Unexpected end of file."
            | Char ('*', Char ('/', xs)) -> Some xs
            | Char (_, xs) -> blockComment xs
        
        function
        | EOL xs | Char (('\t' | ' '), xs) -> Some xs
        | Char ('%', xs) -> lineComment xs
        | Char ('/', Char('*', xs)) -> blockComment xs
        | _ -> None
    
    let (|End|_|) = function
        | Char ('.', Layout xs) -> Some xs
        | _ -> None
    
    let (|Upper|_|) = function
        | Char (x, xs) when x >= 'A' && x <= 'Z' -> Some (x, xs)
        | _ -> None
    
    let (|Lower|_|) = function
        | Char (x, xs) when x >= 'a' && x <= 'z' -> Some (x, xs)
        | _ -> None
    
    let (|Alphanum|_|) = function
        | Upper (x, xs) | Lower (x, xs)
        | Char (x, xs) when '0' <= x && x <= '9' || x = '_' -> Some (x, xs)
        | _ -> None
    
    let (|Graphic|_|) = function
        | Char ('#' | '$' | '&' | '*' | '+' | '-' | '.' | '^' | ':' | '<' |
          '=' | '>' | '?' | '@' | '^' | '~' | '\\' as x, xs) -> Some (x, xs)
        | _ -> None
    
    let (|Integer|_|) b =
        let (|Digit|_|) =
            if b <= 10 then function
                | Char (x, xs) when '0' <= x && x < char (int '0' + b) ->
                    Some (int x - int '0', xs)
                | _ -> None
            else function
                | Char (x, xs) when '0' <= x && x <= '9' ->
                    Some (int x - int '0', xs)
                | Char (x, xs) when 'a' <= x && x < char (int 'a' + b - 10) ->
                    Some (int x - int 'a' + 10, xs)
                | Char (x, xs) when 'A' <= x && x < char (int 'A' + b - 10) ->
                    Some (int x - int 'A' + 10, xs)
                | _ -> None
        
        let rec integer acc = function
            | Digit (x, xs) -> integer (acc * b + x) xs
            | xs -> Some (acc, xs)
        
        function
        | Digit (x, xs) -> integer x xs
        | _ -> None
    
    let (|QuotedChar|_|) delim =
        let close = function
            | Char (x, xs) when x = delim -> Some (delim, xs)
            | xs -> None
        
        let escape = function
            | EOL _ -> failwith "Unexpected new line."
            | Char ('\'' | '"' | '`' as x, xs) -> Some (x, xs)
            | Integer 8 (x, Char ('\\', xs)) -> Some (char x, xs)
            | Char ('x', Integer 16 (x, Char ('\\', xs))) -> Some (char x, xs)
            | Char ('\\', xs) -> Some ('\\', xs)
            | Char ('a', xs) -> Some ('\u0007', xs)
            | Char ('b', xs) -> Some ('\b', xs)
            | Char ('n', xs) -> Some ('\n', xs)
            | Char ('v', xs) -> Some ('\u000B', xs)
            | Char ('f', xs) -> Some ('\u000C', xs)
            | Char ('r', xs) -> Some ('\r', xs)
            | Char ('e', xs) -> Some ('\u001B', xs)
            | EOF -> failwith "Unexpected end of file."
            | _ -> failwith "Invalid escape sequence."
        
        function
        | EOL _ -> failwith "Unexpected new line."
        | Char (x, xs) when x = delim -> close xs
        | Char ('\\', xs) -> escape xs
        | Char (x, xs) -> Some (x, xs)
        | EOF -> failwith "Unexpected end of file."
    
    let quote delim tokenCons =
        let rec quote' acc = function
            | Char ('\\', EOL xs) -> quote' acc xs
            | QuotedChar delim (x, xs) -> quote' (x :: acc) xs
            | Char (x, xs) when x = delim ->
                token (acc |> List.rev |> tokenCons) xs
            | _ -> failwith "Syntax error."
        
        quote' []
    
    let alphanumName tokenCons acc =
        let rec alphanumName' acc = function
            | Alphanum (x, xs) -> alphanumName' (x :: acc) xs
            | xs -> token (acc |> List.rev |> tokenCons) xs
        
        alphanumName' acc
    
    let rec graphicName acc = function
        | Graphic (x, xs) -> graphicName (x :: acc) xs
        | xs -> token (acc |> List.rev |> String.of_chars |> Name) xs
    
    let postLayout = function
        | Char ('(', xs) -> token Open xs
        | xs -> lex xs
    
    match xs with
    | EOF -> eof ()
    | Layout xs -> postLayout xs
    | End xs -> token End xs
    | Char ('!', xs) -> token (Name "!") xs
    | Char ('[', xs) -> token OpenList xs
    | Char ('{', xs) -> token OpenCurly xs
    | Char ('(', xs) -> token OpenCT xs
    | Char (')', xs) -> token Close xs
    | Char (',', xs) -> token Comma xs
    | Char (';', xs) -> token (Name ";") xs
    | Char (']', xs) -> token CloseList xs
    | Char ('|', xs) -> token HTSeparator xs
    | Char ('}', xs) -> token CloseCurly xs
    | Char ('"', xs) -> quote '"' DoubleQuotedList xs
    | Char ('`', xs) -> quote '`' (String.of_chars >> BackQuotedString) xs
    | Char ('\'', xs) -> quote '\'' (String.of_chars >> Name) xs
    | Lower (x, xs) -> alphanumName (String.of_chars >> Name) [x] xs
    | Graphic (x, xs) -> graphicName [x] xs
    | Upper (x, xs) | Char ('_' as x, xs) ->
        alphanumName (String.of_chars >> VariableName) [x] xs
    | Char ('0', Char ('\'', QuotedChar '\'' (x, xs))) ->
        token (IntegerLit (int x)) xs
    | Char ('0', Char ('b', Integer 2 (x, xs)))
    | Char ('0', Char ('o', Integer 8 (x, xs)))
    | Char ('0', Char ('x', Integer 16 (x, xs)))
    | Integer 10 (x, xs) ->
        token (IntegerLit x) xs
    | _ -> failwith "Unexpected character."
