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
