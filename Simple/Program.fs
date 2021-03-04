namespace Simple

open Jslinq.Types

[<Policy>]
module Policy =
    [<SecT("_^H -> _^H")>]
    let foo (_ : int) = 1

    [<SecT("_ ->^L _")>]
    let id (s : int) = s

module Application =
    let bar = id 1
    //Policy.id 1