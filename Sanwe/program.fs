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

module Sanwe.Program

open System
open Sanwe.Common
open Sanwe.Syntax
open Sanwe.Types

module Terp =
    let query s term =
        printfn "Read term: %A" term
        s
    
    let toplevel s readLn printError =
        let rec readLns () =
            match readLn () with
            | None -> LazyList.empty ()
            | Some ln ->
                LazyList.append (LazyList.of_seq ln)
                    (LazyList.consf '\n' readLns)
        
        let rec toplevel' s pos source =
            try
                match parse s pos source with
                | None -> s
                | Some (term, pos', source') ->
                    match term with
                    | Atom "halt" -> s
                    | _ -> toplevel' (query s term) pos' source'
            with
            | SyntaxError (msg, pos, pos', source') ->
                printError msg pos
                toplevel' s pos' source'
        
        toplevel' s { line = 1; col = 1 } (readLns ()) |> ignore

[<EntryPoint>]
let main _ =
    let readLn () =
        printf "> "
        
        match Console.ReadLine () with
        | null -> None
        | ln -> Some ln
    
    let printError msg pos =
        printfn "Syntax error at %d:%d: %s" pos.line pos.col msg
    
    printfn "Sanwe: A Prolog interpreter in F#"
    Terp.toplevel TerpState.initial readLn printError
    
    0
