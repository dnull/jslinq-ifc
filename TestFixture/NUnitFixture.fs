namespace TestFixture

open Jslinq.Types
open NUnit.Framework
open Microsoft.FSharp.Compiler.SourceCodeServices

module TestFixture =

    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    let projOptions = checker.GetProjectOptionsFromProjectFile(@"..\..\..\TestCases\TestCases.fsproj")

    let results =
        checker.ParseAndCheckProject(projOptions)
        |> Async.RunSynchronously

    let rec findSubDeclarations name decl = 
        decl
        |> List.pick(
                function
                | FSharpImplementationFileDeclaration.Entity (e,sub) -> if e.DisplayName = name then Some sub else None
                | _ -> None
            )

    let sub name =
        let tc = findSubDeclarations "TestCases" results.AssemblyContents.ImplementationFiles.Head.Declarations
        findSubDeclarations name tc

    [<TestFixture>]
    type TestClass() = 

        [<Test>]
        member this.Constants() = 

            let types =
                (Jslinq.TypeChecker.processDeclarations defaultEnv (sub "case01")).Γ
                |> List.map snd

            Assert.AreEqual(types.[0], Base (LVar "l1"))
