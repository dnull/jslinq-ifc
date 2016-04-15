namespace Scenarios

open WebSharper.Html.Server
open WebSharper
open WebSharper.Sitelets

open Jslinq.Types

type Action =
    | Home
    | Scenario of Web.Control

// This is the place where each new scenario can be added.
[<Ignore>]
module Scenarios =

    // Unfortunately does WebSharper seem to require a named type for each module.
    [<Sealed>] type LocationIframe() = inherit Web.Control() [<JavaScript>] override __.Body = LocationIframe.Main() :> _
    [<Sealed>] type LocationEmbedded() = inherit Web.Control() [<JavaScript>] override __.Body = LocationEmbedded.Main() :> _
    [<Sealed>] type AreaAggregation() = inherit Web.Control() [<JavaScript>] override __.Body = AreaAggregation.Main() :> _    
    [<Sealed>] type PasswordMeter() = inherit Web.Control() [<JavaScript>] override __.Body = PasswordMeter.Main() :> _    
    [<Sealed>] type Battleship() = inherit Web.Control() [<JavaScript>] override __.Body = Battleship.Main() :> _    

    let scenarioList =
        [
            (new LocationIframe() :> Web.Control, "Location-based service (IFrame)")
            (new LocationEmbedded() :> Web.Control, "Location-based service (Embedded)")
            (new AreaAggregation() :> Web.Control, "Area Aggregation / Movie Rental")
            (new PasswordMeter() :> Web.Control, "Password Meter")
            (new Battleship() :> Web.Control, "Battleship")
        ]

[<Ignore>]
module Skin =
    open System.Web

    type Page =
        {
            Title : string
            Body : list<Element>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let WithTemplate title body : Content<Action> =
        Content.WithTemplate MainTemplate <| fun context ->
            {
                Title = title
                Body = body context
            }

[<Ignore>]
module Site =
    let ( => ) text url =
        A [HRef url] -< [Text text]

    let Links (ctx: Context<Action>) =
        UL [] -< (Scenarios.scenarioList |> List.map (fun (ep, title) -> LI [title => ctx.Link (Action.Scenario ep)]))

    let HomePage =
        let title = "JSLINQ Scenarios"
        Skin.WithTemplate title <| fun ctx ->
            [
                Div [Text title]
                Links ctx
            ]

    let ScenarioPage (entryPoint, title) =
        Skin.WithTemplate title <| fun ctx ->
            [
                Div ["Home" => ctx.Link Action.Home]
                Div [Text title]
                Div [entryPoint]

            ]

    let MainSitelet =
        Sitelet.Sum (
            
            (Sitelet.Content "/" Action.Home HomePage) ::
            (Scenarios.scenarioList |> List.map (fun (ep, title) -> Sitelet.Content (ep.GetType().Name) (Action.Scenario ep) (ScenarioPage (ep, title)))
            ))

[<Ignore>]
module SelfHostedServer =

    open global.Owin
    open Microsoft.Owin.Hosting
    open Microsoft.Owin.StaticFiles
    open Microsoft.Owin.FileSystems
    open WebSharper.Owin

    [<EntryPoint>]
    let Main = function
        | [| rootDirectory; url |] ->
            use server = WebApp.Start(url, fun appB ->
                appB.UseStaticFiles(
                        StaticFileOptions(
                            FileSystem = PhysicalFileSystem(rootDirectory))).UseCors(Microsoft.Owin.Cors.CorsOptions.AllowAll)
                    .UseSitelet(rootDirectory, Site.MainSitelet)
                |> ignore)
            stdout.WriteLine("Serving {0}", url)
            stdin.ReadLine() |> ignore
            0
        | _ ->
            eprintfn "Usage: Scenarios ROOT_DIRECTORY URL"
            1
