namespace Scenarios

open Jslinq.Types
open WebSharper

open DB
open JsLib

module AreaAggregation =

    type ItemResult = {item:string; eventCount:int}

    type AreaBox =
        {
            origLat : float
            origLon : float
            offset : float    // Offset added to lat and lon
        }
  
    [<Remote>]
    [<SecT("_ -> {item:_^L; eventCount:_^L} list^L")>]
    let GetAreaRanking (areaIndex : int) =

        // Base for grid overlay. Set this to your area and do not forget to
        // also generate an example database with a center in this area.
        let gridBase = {origLat=45.0; origLon=90.0; offset=0.025}

        let areas =
            [0;1]
            |> List.map (fun a -> 
                [0;1;2;3]
                |> List.map (fun b ->
                    {gridBase with
                        origLon = gridBase.origLon+(float b)*gridBase.offset
                        origLat = gridBase.origLat+(float a)*gridBase.offset})
                )
            |> List.concat
            
        // Pick selected area, which provides boundaries for DB query
        let box = List.nth areas areaIndex

        let latOffset = box.origLat + box.offset
        let lonOffset = box.origLon + box.offset

        let events =
            query {
                for e in db.Event do
                for i in db.Item do
                if ((e.ItemId = i.Id) &&
                    (e.Lat > box.origLat) && 
                    (e.Lon > box.origLon) &&
                    (e.Lat < latOffset) &&
                    (e.Lon < lonOffset)) then
                    yield i.Name // Allowed
                    //yield (string e.Lat) // Blocked
            }

        let aggregate = GroupStringList (Seq.toList events)

        List.map (fun x -> {item=(fst x);eventCount=(snd x)}) aggregate

    [<JavaScript>]
    let run (selection : Html.Client.Element) output =
        let results = GetAreaRanking (int (GetElementValue selection))
        let resultElements =
            List.map (fun p -> Div [Text (p.item + ": " + string p.eventCount)] :> WebSharper.Html.Client.Pagelet) results

        SetContent output (Div resultElements)

    [<JavaScript>]
    let Main () =
        let output = Div [Text "Ready"]
        let selection =
            Select [
                "Area 1"; "Area 2"; "Area 3"; "Area 4";
                "Area 5"; "Area 6"; "Area 7"; "Area 8"
            ]

        Div [
            selection
            Button [Text "Run"] |>! OnClick (fun () -> run selection output)     
            output   
        ]
