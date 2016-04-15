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

open WebSharper
open Jslinq.Types

[<Ignore>]
[<JavaScript>]
// Content of this module will be handled with Rule OP.
// This is necessary, since the join of two levels cannot be expressed in the
// type signature at the moment.
module JsOp =
    open WebSharper.JavaScript

    // Reference for Google Map. Done here so that we do not have to support "Map" type.
    let GoogleMap = ref null

    let testRegExp exp text = RegExp(exp).Test(text)

    // PRNG shared by all following modules
    let rng = System.Random()

    // Dirty trick to fix security type check for these intrinsical operators.
    let ( || ) (a:bool) (b:bool) = a || b
    let ( && ) (a:bool) (b:bool) = a && b

[<Ignore>]
module SrvOp =
    // For now we assume that distance is also confidential, so use rule OP.
    let distance aLat aLon bLat bLon =
        let a = System.Device.Location.GeoCoordinate(aLat, aLon)
        let b = System.Device.Location.GeoCoordinate(bLat, bLon)
        a.GetDistanceTo(b)

[<Policy>]
module SrvLib =
    [<SecT("_ -> unit")>]
    let LogS (s:string) = System.Console.WriteLine s

[<Policy>]
[<JavaScript>]
// Content of this module represents policy, thus type signatures for value/function declarations
// are mandatory.
module JsLib =
    open WebSharper.JavaScript
    open WebSharper.Html.Client
    open WebSharper.Google.Maps

    // The following signature expresses declassification, since it is not checked
    // against the inferred security type.
    // In a normal module this would not typecheck when the function has high input.
    //
    // Note: It is important that the input is not restricted (e.g. H), since then it
    // would upgrade the classified value as well.
    [<SecT("_ -> _^L")>] 
    let addRandomOffset (x : float) =
        // This is not really secure. An attacker could just multiply the actual value with a number like 10000
        // and divide it again on when it is disclosed. The added random offset is then irrelevant.
        // One solution might be to make plausibility checks, to make sure this really is a coordinate value.
        x + (JsOp.rng.NextDouble() - 0.5) * 0.01

    [<SecT("_^'a -> _^'a")>]
    let RandInt x = JsOp.rng.Next(x)

    [<SecT("_ list -> _")>]
    let Div (x:Pagelet list) = Div x

    [<SecT("_ -> _ -> _")>]
    let IFrame (w : int) (h : int) = IFrame [Width (string w); Height (string h)]

    [<SecT("unit -> _^H")>]
    let Checkbox () = Input [Attr.Type "checkbox"]

    [<SecT("_ -> _ -> unit")>]
    let SetContent (e:Element) (x:Pagelet) =
        e.Clear ()
        e.Append x

    // Simple declassification used by Battleship example
    [<SecT("_ -> _")>]
    let DeclassifyBool (x:bool) = x

    // Shortcut for list generation (not hardcoded in JSLINQ)
    [<SecT("_^'a -> _ list^'a")>]
    let GenList (x:int) = [0..x]

    [<SecT("_ -> unit")>]
    let Disable (e:Element) = e.SetAttribute("disabled", "true")

    [<SecT("_^'a -> _^'a")>]
    let Text (x:string) = Text x

    [<SecT("_^L ->^L _")>]
    let Src (x:string) = Src x

    [<SecT("unit -> _^H")>]
    let InputPW () = Input [Attr.Type "password"]

    [<SecT("unit -> _^H")>]
    let InputH () = Input []

    [<SecT("_ list -> _")>]
    let Button (x:Pagelet list) = Button x

    [<SecT("_ list -> _")>]
    let GridButton (x:Pagelet list) =
        Button ([Attr.Style "height:30px; width:30px"] @ x)

    // Stylesheets have low side effects, similar to Src
    [<SecT("_ ->^L _^L")>]
    let Style (css : string) = Attr.Style css

    [<SecT("_^'b -> (_ -> unit) -> _^'b")>]
    let ( |>! ) (x : Element) y = ( |>! ) x y

    [<SecT("(unit -> unit) -> _ -> unit")>]
    let OnKeyDown (f : unit -> unit) x = OnKeyDown (fun _ _ -> f ()) x

    [<SecT("_^'c -> _^'c")>]
    let GetElementValue (x : Element) =
        match x.Dom.NodeName with
        | "INPUT" | "SELECT" -> x.Value
        | _ -> "" // Not very clean, but prevents to read value from trusted sinks. 

    [<SecT("(unit -> unit) -> _ -> unit")>]
    let OnClick (f : unit -> unit) x = OnClick (fun _ _ -> f ()) x

    [<SecT("(_^H -> _^H -> unit) -> unit")>]
    let GetPosition (callback : float -> float -> unit) =
        let options = Geolocation.PositionOptions (EnableHighAccuracy=true)
        let callbackAdapter (pos : Geolocation.Position) = callback pos.Coords.Latitude pos.Coords.Longitude
        JS.Window.Navigator.Geolocation.GetCurrentPosition(callbackAdapter, ignore, options)

    [<SecT("unit ->^L _^L")>]
    let InitGoogleMapDiv () =
        Div [Attr.Style "padding-bottom:20px; width:500px; height:300px;"]
        |>! OnAfterRender (fun mapElement ->
            let center = LatLng(45.0, 90.0)
            let options = MapOptions(center, 8)
            let map = new Google.Maps.Map(mapElement.Dom, options)
            JsOp.GoogleMap := map)

    [<SecT("_^L -> _^L ->^L unit")>]
    let PanGoogleMap lat lon =
        (!JsOp.GoogleMap).PanTo(LatLng(lat, lon))

    [<SecT("_ list -> _")>]
    let Image (x:Pagelet list) = Img x

    [<SecT("_ -> unit")>]
    let Alert (s : string) = JS.Alert s

    // Allow the user to make a selection from a list of strings. Returns only the index of the selected element
    // so that this element does not leak information and can be a trusted sink.
    [<SecT("_ list^'a -> _^'a")>]
    let Select (strings : string list) =
        Select [] -<
            (strings
            |> List.mapi (fun i s -> Tags.Option [Text s; Attr.Value (string i)]))

    // Grouping the list is abstracted away like this, so that we do not have
    // to support all these Seq and List operations.
    [<SecT("_^'a list -> (_^'a * _) list")>]
    let GroupStringList (strings : string list) =
        strings
        |> List.toSeq
        |> Seq.groupBy id
        |> Seq.map (fun (s, strings) -> (s, Seq.length strings))
        |> Seq.sortBy snd
        |> Seq.toList
        |> List.rev

[<Ignore>]
module DB =

    // Keyword: Attribute-Based mapping
    // http://mark.mymonster.nl/2008/11/03/linq-to-sql-doing-it-manually-part-1-table-and-column-creation-from-c/
    // http://mark.mymonster.nl/2008/11/09/linq-to-sql-doing-it-manually-part-2-database-creation
    open System.Data.Linq
    open System.Data.Linq.Mapping

    [<Table>]
    [<SecT("_^L")>]
    type Points() =
        let mutable m_id : int = 0
        let mutable m_latitude : float = 0.0
        let mutable m_longitude : float = 0.0
        let mutable m_note : string = null

        [<Column>]
        [<SecT("_^L")>]
        member this.Id
            with get() = m_id
            and set(x) = m_id <- x

        [<Column>]
        [<SecT("_^L")>]
        member this.Latitude
            with get() = m_latitude
            and set(x) = m_latitude <- x

        [<Column>]
        [<SecT("_^L")>]
        member this.Longitude
            with get() = m_longitude
            and set(x) = m_longitude <- x

        [<Column>]
        [<SecT("_^L")>]
        member this.Note
            with get() = m_note
            and set(x) = m_note <- x

    [<Table>]
    [<SecT("_^L")>]
    type Event() =
        let mutable m_id : int = 0
        let mutable m_latitude : float = 0.0
        let mutable m_longitude : float = 0.0
        let mutable m_itemId : int = 0
        let mutable m_userId : int = 0

        [<Column>]
        [<SecT("_^L")>]
        member this.Id
            with get() = m_id
            and set(x) = m_id <- x

        [<Column>]
        [<SecT("_^H")>]
        member this.Lat
            with get() = m_latitude
            and set(x) = m_latitude <- x

        [<Column>]
        [<SecT("_^H")>]
        member this.Lon
            with get() = m_longitude
            and set(x) = m_longitude <- x

        [<Column>]
        [<SecT("_^L")>]
        member this.ItemId
            with get() = m_itemId
            and set(x) = m_itemId <- x

    [<Table>]
    [<SecT("_^L")>]
    type Item() =
        let mutable m_id : int = 0
        let mutable m_name : string = null

        [<Column>]
        [<SecT("_^L")>]
        member this.Id
            with get() = m_id
            and set(x) = m_id <- x

        [<Column>]
        [<SecT("_^L")>]
        member this.Name
            with get() = m_name
            and set(x) = m_name <- x

    type MyDataContext(c : string) =
        inherit DataContext(c)
        member this.Points : Table<Points> = this.GetTable<Points>()
        member this.Event : Table<Event> = this.GetTable<Event>()
        member this.Item : Table<Item> = this.GetTable<Item>()

    let cstring = System.Configuration.ConfigurationManager.ConnectionStrings.Item("jslinq-db")
    let db = new MyDataContext(cstring.ConnectionString)