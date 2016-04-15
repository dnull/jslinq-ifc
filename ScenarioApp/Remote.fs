namespace ScenarioApp

open WebSharper
open Jslinq.Types

// Open database policy
//open DB

module Remote =
    [<SecT("_^L ->^L _")>]
    [<Remote>]
    let lookup (hash : string) =
//        let matches = 
//            query {
//                for u in db.AppUser do
//                if u.PhoneHash = hash then
//                    yield ()
//            }
//
//        if LibServer.IsEmpty (Seq.toList matches) then true else false
        
        // Dummy lookup without database for "0123 45 67 89"
        if hash = "870a4998728e86771ae52cf3ea0f773b" then true else false