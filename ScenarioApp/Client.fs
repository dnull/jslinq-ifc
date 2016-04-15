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