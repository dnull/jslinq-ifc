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