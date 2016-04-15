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
open WebSharper.Html.Client

open Jslinq.Types

open DB
open JsLib
open SrvLib
open JsOp

module Battleship =

    // A simple battleship-style game

    type GameParameters = {size: int; ships: int list}

    type Position = {x:int; y:int}

    type ServerCell =
        {
            tested:bool ref;
            occupied:bool ref;
            position:Position
        }

    type ServerState =
        {
            localGrid: ServerCell list list;
            remoteGrid: ServerCell list list
        }

    type ClientCell =
        {
            tested:bool ref;
            occupied:bool ref;
            element:Element
        }

    type ClientState =
        {
            localGrid: ClientCell list list;
            remoteGrid: ClientCell list list
        }    

    type Response =
        {
            hit:bool;
            shot:Position;
            defeated:bool
        }
    
    // Hard-coded game parameters
    [<Remote>]
    [<SecT("unit ->^L {size: _^L; ships: _^L list^L}")>]
    let Parameters () =
        {
            // Size of the grid
            size=10

            // Ships that can be placed (simplified to avoid collisions)
            //ships = [5;4;4;3;3;3;2;2;2;2]
            ships = [5;3;2]    
        }

    // Server-side game state. For simplicity we only maintain one state and
    // do not differentiate between different games/sessions.
    [<SecT("{
        localGrid: {
            tested:_^L ref;
            occupied:_^H ref;
            position:{x:_^L;y:_^L}} list^L list^L;
        remoteGrid: {
            tested:_^L ref;
            occupied:_^L ref;
            position:{x:_^L;y:_^L}} list^L list^L
        }")>]
    let serverState : ServerState =
        let param = Parameters ()
        let ints = GenList (param.size-1)
        {
            // local and remote grid cannot be filled by the same function,
            // otherwise does JSLINQ complain.

            localGrid =
                List.map (fun i ->
                    List.map (fun j ->
                        {tested=ref false;
                            occupied=ref false;
                            position={x=i;y=j}}) ints) ints

            remoteGrid =
                List.map (fun i ->
                    List.map (fun j ->
                        {tested=ref false;
                            occupied=ref false;
                            position={x=i;y=j}}) ints) ints
        }

    // Helper to get array element
    let getCell (p:Position) (g:ServerCell list list) =
        (List.nth (List.nth g p.x) p.y)

    // Helper to determine grid cells occupied by a random ship
    [<JavaScript>]
    let placeShip (x:int) (p:GameParameters) =
        let orientation = ((RandInt 2) = 0)
        let head = (RandInt (p.size - x))
        let y = (RandInt p.size)
        GenList (x-1)
        |> List.map (fun x -> x+head)
        |> List.map (
            fun x ->
                let result =
                    if orientation then {x=x; y=y}
                    else {x=y; y=x}
                result
            )

    // Resets the game on the server and retrieves parameters
    [<Remote>]
    [<SecT("unit ->^L {size: _^L; ships: _^L list^L}")>]
    let Initialize () =
        // Clear state
        List.map (fun (x:ServerCell list) ->
            (List.map (fun (y:ServerCell) ->
                y.occupied := false
                y.tested := false
                ()
                ) x)) serverState.localGrid |> ignore

        // Get parameters
        let param = Parameters ()

        // Populate cells with randomly placed ships
        param.ships
        |> List.map (fun x ->
            placeShip x param
            |> List.map (
                fun x ->
                    let c = getCell x serverState.localGrid
                    c.occupied := true
                )
            ) |> ignore

        // Return game parameters
        param

    // Used by the client to report hit/miss to server
    [<Remote>]
    [<SecT("{hit:_^L; shot:{x:_^L; y:_^L}; defeated:_^L} ->^L unit")>]
    let Report (r:Response) =
        let target = getCell r.shot serverState.remoteGrid
        target.tested := true
        target.occupied := r.hit
        
    [<Remote>]
    [<SecT("{x:_^L; y:_^L} ->^L {hit:_^L; shot:{x:_^L; y:_^L}; defeated:_^L}")>]  
    let Play (s:Position) : Response =
        // Get client target cell
        let clientTarget = getCell s serverState.localGrid

        // Mark cell as tested by user
        clientTarget.tested := true

        // Is server defeated?
        let defeated =
            serverState.localGrid
            |> List.concat
            |> List.forall
                (fun x -> (not !x.occupied) || (!x.occupied && !x.tested))

        // Create random shot as answer
        let untestedCells =
            serverState.remoteGrid
            |> List.concat
            |> List.filter (fun x -> not !x.tested)
        let untestedRandomCell =
            List.nth untestedCells (RandInt (List.length untestedCells))

        // Return result
        {
            shot = untestedRandomCell.position
            hit = DeclassifyBool (!clientTarget.occupied);
            defeated = DeclassifyBool defeated
        }

    // List of integers according to grid size
    [<JavaScript>]
    let newJsInts () =
        let param = Parameters ()
        GenList (param.size-1)

    // Creates a new client state. Put in top-level function so that it
    // can be annotated with security type signature.
    [<JavaScript>]
    [<SecT("unit -> {
        localGrid:{
            tested:_^L ref;
            occupied:_^H ref;
            element:_^H} list^L list^L;
        remoteGrid:{
            tested:_^L ref;
            occupied:_^L ref;
            element:_^L} list^L list^L
        }")>]
    let newClientState () : ClientState =
        let jsInts = newJsInts ()
        {
            localGrid =
                List.map (fun _ ->
                    List.map (fun _ ->
                        {tested=ref false;
                            occupied=ref false;
                            element=GridButton [Text "."]}) jsInts) jsInts

            remoteGrid =
                List.map (fun _ ->
                    List.map (fun _ ->
                        {tested=ref false;
                            occupied=ref false;
                            element=Div [Text ""] (* dummy *)}) jsInts) jsInts
        }

    [<JavaScript>]
    let Main () =
        // Initialize a new game
        let param = Initialize ()

        // Initialize client state
        let clientState = newClientState ()

        let jsInts = newJsInts ()

        // Helpers for list access
        let getCell (p:Position) (g:ClientCell list list) =
            (List.nth (List.nth g p.x) p.y)

        // Populate local grid with random ships
        param.ships
        |> List.map (fun x ->
            placeShip x param
            |> List.map (
                fun x ->
                    let c = getCell x clientState.localGrid
                    // Set visible element
                    SetContent c.element (Text "O")
                    // Set information in matrix
                    c.occupied := true
                )
            ) |> ignore

        // Check game state, send shot to server and process responding move 
        // from server.
        let sendShot (x:int) (y:int) (b:Element)=
            let shot =  {x=x; y=y}
            let response = Play shot

            // Record effect of own shot
            let newText =
                if response.hit then "X"
                else "-"
            SetContent b (Text newText)
            Disable b

            // Show end of game when server reports defeat
            if response.defeated then Alert "Server defeated!"

            // Record shot of server
            let serverTarget = getCell response.shot clientState.localGrid
            serverTarget.tested := true
            Disable serverTarget.element

            // Determine if client is defeated by server
            let clientDefeated =
                clientState.localGrid
                |> List.concat
                |> List.forall
                    (fun x -> (not !x.occupied) || (!x.occupied && !x.tested))

            // Report hit/miss of server's shot to server.
            let serverShotResult =
                // Record for response to server
                {
                    shot = response.shot;
                    hit = DeclassifyBool !serverTarget.occupied;
                    defeated = DeclassifyBool clientDefeated;
                }

            Report serverShotResult
            if clientDefeated then Alert "You have been defeated by the server."

        let remoteGrid : Pagelet list list =
            // User wants to make a shot
            let shootButton (x:int) (y:int) =
                let b = GridButton [Text (string "?")]
                b
                |>! OnClick (fun () -> sendShot x y b)

            List.map (fun i ->
                List.map (fun j -> shootButton j i :> Pagelet) jsInts) jsInts

        let localGridPagelets =
            clientState.localGrid
            |> List.map (fun x -> (List.map (fun y -> y.element :> Pagelet) x))

        Div [
            Div [
                Text "Game Grid";
                Div (List.map (fun x -> Div x :> Pagelet) localGridPagelets);
                Style "float: left; margin: 1em"]

            Div [
                Text "Tracking Grid";
                Div (List.map (fun x -> Div x :> Pagelet) remoteGrid);
                Style "float: left; margin: 1em"]
        ]
