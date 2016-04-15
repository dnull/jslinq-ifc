namespace Scenarios

open Jslinq.Types
open WebSharper

module LocationEmbedded =

    open DB
    open JsLib

    open LocationIframe

    [<JavaScript>]
    let Locate out =
        let callback (lat : float) (lon : float) =

            let declassLat = addRandomOffset lat
            let declassLon = addRandomOffset lon

            // Get POIs from application server
            let pois = GetPois declassLat declassLon
            let poiElements = List.map (fun p -> Div [Text (p.note + " (" + string p.dist + "m)")] :> WebSharper.Html.Client.Pagelet) pois

            PanGoogleMap declassLat declassLon
            SetContent out (Div poiElements)

        GetPosition callback

    

    [<JavaScript>]
    let Main () =
        let output = Div [Text "Ready."]
        let mapDiv = InitGoogleMapDiv ()
        Div [
            Button [Text "Locate!"] |>! OnClick (fun () -> Locate output)
            mapDiv
            output
        ]