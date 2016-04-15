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

namespace TestCases

open Jslinq.Types
open System.Data.Linq
open System.Data.Linq.Mapping

[<Ignore>]
module DB =
    [<Table>]
    [<SecT("_^L")>]
    type Table =

        [<Column>]
        [<SecT("_^H")>]
        abstract member High : bool

        [<Column>]
        [<SecT("_^L")>]
        abstract member Low : bool

    type Context() =
        inherit DataContext("")
        member this.Table : Table<Table> = this.GetTable<Table>()

    let ctx = new Context()

[<Ignore>]
module case01 =
    type twoBools = {a:bool;b:bool}

    let qBaseL = query { for x in DB.ctx.Table do yield x.Low }
    let qBaseH = query { for x in DB.ctx.Table do yield x.High }
    let qTupleLH = query { for x in DB.ctx.Table do yield (x.Low, x.High) }
    let qRecLH = query { for x in DB.ctx.Table do yield {a=x.Low; b=x.High} }

[<Ignore>]
module case02 =
    [<SecT("_^H")>]
    let boolH = true

    let qIfH = query { for x in DB.ctx.Table do if boolH then yield x.Low }
    let qIfH' = query { for x in DB.ctx.Table do if x.High then yield x.Low }
    let qIfH'' = query { for x in DB.ctx.Table do if (x.High && false) then yield x.Low }

[<Ignore>]
module case03 =
    [<SecT("_^H")>]
    let boolH = true

    let qTwoFors =
        query { 
            for x in DB.ctx.Table do
            for y in DB.ctx.Table do
            if x.High = y.High then yield x.Low
        }

[<Ignore>]
module case04 =
    [<Policy>]
    module Policy =
        [<SecT("_ list -> (_^L * _^L) list^L")>]
        let GroupStringList (strings : string list) =
            strings
            |> List.toSeq
            |> Seq.groupBy id
            |> Seq.map (fun (s, strings) -> (s, Seq.length strings))
            |> Seq.sortBy snd
            |> Seq.toList
            |> List.rev

    let list = ["A";"A";"B"]
    let grouping = Policy.GroupStringList list

[<Ignore>]
module case05 =
    [<SecT("_^H")>]
    let boolH = true

    [<SecT("_^L")>]
    let boolL = true

    // F# turns the && operation into an if-statement in this case.
    let qIfLH = if (boolL && boolH) then 1 else 0
    let qIfHL = if (boolH && boolL) then 1 else 0
    let IfAndHL = if boolH then boolL else false
    let IfAndLH = if boolL then boolH else false
    
[<Ignore>]
module case06 =
    [<SecT("_^'a ->^'a _^'a ->^'a unit")>]
    let a (x:int) (y:int) = ()

    [<SecT("_^'a ->^'a _^'a ->^'a unit")>]
    let b (x:int) (y:int) = ()

// TODO: Make this test case work. At the moment constraints are not fully resolved.
[<Ignore>]
module case07 =
    [<Policy>] 
    module Policy =
        [<SecT("_ -> _")>]    
        let declassify (x:bool) = false

    open Policy

    [<SecT("_^H")>]    
    let secret = true

    [<SecT("_^L ->^L unit")>]    
    let leak (x:bool) = ()

    let func () = declassify secret

    let explicit = leak (declassify secret)

    let implicit =
        leak (
            if secret
            // Also not captured when "declassify" is removed
            then (declassify true)
            else (declassify false))

// Expected to fail, since same ref cell is used for two different security classes.
[<Ignore>]
module case08 =
    // Custom types
    type ServerCell = {occupied:bool ref}
    type ServerState =
        {
            localGrid: ServerCell list list;
            remoteGrid: ServerCell list list
        }

    [<SecT("unit -> {localGrid: ({occupied:_^H ref} list^L) list^L; remoteGrid: ({occupied:_^L ref} list^L) list^L}")>]  
    let record () =
        let defaultCell = {occupied=ref false}
        {localGrid = [[defaultCell]]; remoteGrid = [[defaultCell]]}
