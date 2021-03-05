namespace Simple

open Jslinq.Types

[<Policy>]
module Policy =
    [<SecT("_^H -> _^H")>]
    let sourceH (_ : int) = 1

    [<SecT("_^L -> _")>]
    let sinkL (s : int) = s

module Application =
    let constInt = 1
    let valH = Policy.sourceH 1
    let computation1 = Policy.sinkL (Policy.sourceH 1) // MatchFailureException when removed
    let computation2 = Policy.sinkL valH