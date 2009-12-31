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

module Sanwe.Common

open System
open System.Text

let identity a = a

let flip f a b = f b a

let curry f a b = f (a, b)

let curry3 f a b c = f (a, b, c)

let curry4 f a b c d = f (a, b, c, d)

let uncurry f (a, b) = f a b

let uncurry3 f (a, b, c) = f a b c

let uncurry4 f (a, b, c, d) = f a b c d

let tuple a b = curry Tuple.Create a b

let tuple3 a b c = curry3 Tuple.Create a b c

let tuple4 a b c d = curry4 Tuple.Create a b c d

module String =
    let ofChars xs =
        (StringBuilder ()
         |> List.fold (fun sb -> sb.Append : char -> _)
         >> string) xs

module Int32 =
    let ofChars b xs = Convert.ToInt32 (String.ofChars xs, (b : int))

module Double =
    let ofChars = String.ofChars >> Double.Parse
