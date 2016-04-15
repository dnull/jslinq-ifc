//   Copyright 2016 Benjamin Liebe
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

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