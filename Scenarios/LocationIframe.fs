namespace Scenarios

open Jslinq.Types
open WebSharper

module LocationIframe =

    open DB
    open JsLib

    type PoiResult =
        {
            lat : float
            lon : float
            note : string
            dist: float
        }

    [<Remote>]
    [<SecT("_^L -> _^L ->^L {lat:_; lon:_; note:_; dist:_} list")>]
    let GetPois (refLat:float) (refLon:float) =
        let result =
            query {
                for p in db.Points do
                yield ({lat=p.Latitude; lon=p.Longitude; note=p.Note; dist=0.0})
            }
        // Perform distance calculation outside of query, since this better reflects
        // language from paper. Can actually also be done inside query {...}.
        Seq.toList result
        |> List.map (fun r -> {r with dist = (SrvOp.distance refLat refLon r.lat r.lon)})
        |> List.sortBy (fun r -> r.dist)

    [<JavaScript>]
    let Locate out mapFrame =
        let callback (lat : float) (lon : float) =

            let declassLat = addRandomOffset lat
            let declassLon = addRandomOffset lon

            let mapUrl =
                "https://www.google.com/maps/embed/v1/search?q=" +
                (string declassLat) + "," +
                (string declassLon) +
                "&key=AIzaSyDTXkIzkMcdfsaA5yku68pDkwkWdQ9dhjE"

            // Get POIs from application server
            let pois = GetPois declassLat declassLon
            let poiElements = List.map (fun p -> Div [Text (p.note + " (" + string p.dist + "m)")] :> WebSharper.Html.Client.Pagelet) pois

            SetContent out (Div poiElements)
            SetContent mapFrame (Src mapUrl)

        GetPosition callback

    [<JavaScript>]
    let Main () =
        let output = Div [Text "Ready."]
        let mapFrame = IFrame 400 200
        Div [
            Button [Text "Locate!"] |>! OnClick (fun () -> Locate output mapFrame)
            mapFrame
            output
        ]