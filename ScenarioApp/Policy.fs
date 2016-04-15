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

type Contact = {Name : string; Phone : string}

[<Policy>]
module LibServer =
    [<SecT("unit list^'a -> _^'a")>]
    let IsEmpty (l:unit list) = l.IsEmpty


[<Policy>]
[<JavaScript>]
// Content of this module represents policy, thus type signatures for value/function declarations
// are mandatory.
module LibJs =
    open WebSharper.JavaScript
    open WebSharper.Html.Client
    open WebSharper.PhoneGap

    // Declassifies value using a hash function.
    [<SecT("_ -> _")>]
    let Hash (s:string) = string (WebSharper.CryptoJS.MD5 s)

    // Provides a classified contact list.
    [<SecT("({Name:_^H; Phone:_^H} list -> unit) -> unit")>]
    let GetPhoneContacts (handler : Contact list -> unit) =

        /// Callback for asynchronous find() function
        let onFound (cts: Contacts.Contact []) =
            let contacts =
                cts
                |> Array.toSeq
                |> Seq.filter (fun c -> As c.name.formatted)
                |> Seq.distinctBy (fun c -> c.name.formatted)
                |> Seq.filter (fun c -> not (c.phoneNumbers = null) && (c.phoneNumbers.Length > 0))
                |> Seq.sortBy (fun c -> c.name.formatted.ToLower())
                |> Seq.map (fun c -> {Name=c.name.formatted; Phone=c.phoneNumbers.[0].value})
                |> Seq.toList
            handler contacts

        PhoneGap.Contacts.getPlugin().find(
            [| "name"; "phoneNumbers" |],
            onFound,
            ignore,
            Contacts.FindOptions(multiple = true))
//        handler [{Name="A";Phone="0123456789"}]


    [<SecT("_ list -> _")>]
    let Div (x:Pagelet list) = Div x

    [<SecT("_ -> _ -> unit")>]
    let SetContent (e:Element) (x:Pagelet) =
        e.Clear ()
        e.Append x

    [<SecT("_^'a -> _^'a")>]
    let Text (x:string) = Text x

    [<SecT("_ list -> _")>]
    let Button (x:Pagelet list) = Button x

    [<SecT("_^'b -> (_ -> unit) -> _^'b")>]
    let ( |>! ) (x : Element) y = ( |>! ) x y

    [<SecT("(unit -> unit) -> _ -> unit")>]
    let OnClick (f : unit -> unit) x = OnClick (fun _ _ -> f ()) x

[<Ignore>]
module DB =

    open System.Data.Linq
    open System.Data.Linq.Mapping

    [<Table>]
    [<SecT("_^L")>]
    type AppUser() =
        let mutable m_id : int = 0
        let mutable m_name : string = null
        let mutable m_phonehash : string = null

        [<Column>]
        [<SecT("_^L")>]
        member this.Id
            with get() = m_id
            and set(x) = m_id <- x

        [<Column>]
        [<SecT("_^H")>]
        member this.Name
            with get() = m_name
            and set(x) = m_name <- x

        [<Column>]
        [<SecT("_^L")>]
        member this.PhoneHash
            with get() = m_phonehash
            and set(x) = m_phonehash <- x

    type MyDataContext(c : string) =
        inherit DataContext(c)
        member this.AppUser : Table<AppUser> = this.GetTable<AppUser>()

    let cstring = System.Configuration.ConfigurationManager.ConnectionStrings.Item("hash-db")
    let db = new MyDataContext(cstring.ConnectionString)