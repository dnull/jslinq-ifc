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

[<JavaScript>]
module App =
    open LibJs

    let outDiv = Div [ Text "Result." ]
  
    let handler contacts =
        let contactElements =
            List.map (fun c -> Div [Text (c.Name + ": " + string (Remote.lookup (Hash c.Phone)))] :> WebSharper.Html.Client.Pagelet) contacts

        SetContent outDiv (Div contactElements)

    let lookupContacts () = GetPhoneContacts handler
    
    let mainDiv =
        Div [
            Button [ Text "Check contacts!" ]
            |>! OnClick (lookupContacts)
            outDiv
        ] 

[<JavaScript>]
[<Ignore>]
module Client =
    let Main =
        // Set base URL for RPC calls.
        // Needs to be updated to point to your running instance of Scenarios.exe
        Remoting.EndPoint <- "http://localhost:9000/"
        App.mainDiv |> fun res -> res.AppendTo "entrypoint"