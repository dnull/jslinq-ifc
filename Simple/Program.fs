namespace Simple

open System
open Jslinq.Types

[<Policy>]
module Policy =
    [<SecT("_ -> unit")>]
    let LogS (s:string) = System.Console.WriteLine s


module Main =
    [<EntryPoint>]
    let main _ =
        Policy.LogS "Foobar"
        0 // return an integer exit code