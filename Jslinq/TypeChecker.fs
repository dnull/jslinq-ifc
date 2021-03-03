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

namespace Jslinq

module TypeChecker =
    open Types
    open Solve
    open FSharp.Compiler.SourceCodeServices

    /// Counter for functions, used to add prefix to type variables so that they are unique
    let functionCounter = ref 0

    /// Checker instance of F# Compiler Services
    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    /// Processes declarations of a file.
    let processDeclarations (env : Environment) (decls : FSharpImplementationFileDeclaration list) =
        
        /// Processes a declaration. Returns a list of function names and their expression.
        /// Is recursive, since declarations can be nested.
        let rec processDeclaration isPolicy (env : Environment) decl = 
            match decl with 

            // Entities are only used to recurse into, since they can contain expressions.
            | FSharpImplementationFileDeclaration.Entity (e,subDecls) ->

                match SignatureParser.tryGetAttributeArguments<Ignore>(e.Attributes) with
                | Some _ -> env
                | None ->
                    let hasPolicyAttribute =
                        SignatureParser.tryGetAttributeArguments<Policy>(e.Attributes)
                        |> Option.isSome

                    let isPolicy = hasPolicyAttribute || isPolicy
                    subDecls
                    |> List.fold (processDeclaration isPolicy) env

            // MemberOrFunctionOrValues contain expressions. Therefore process them from top to bottom.
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, parameters, e) -> 

                // We are only interested in user-defined expressions which are functions or values.
                if v.IsCompilerGenerated || (not v.IsModuleValueOrMember) then env else

                    // Try to obtain security type signature (if it exists)
                    let prefix = sprintf "%i-" !functionCounter
                    incr functionCounter
                    let userSecType = SignatureParser.tryGetUserSecType prefix (v.Attributes)    

                    // If this is policy, just add specified type to context and do nothing else.
                    if isPolicy then
                        match userSecType with
                        | Some (UserSupplied t) ->
                            // TODO: Check that this type actually matches the signature!
                            {env with Γ = (v, t) :: env.Γ}
                        | None -> failwith (sprintf "Function %s in policy has no type annotation." v.DisplayName)
                    else
                        // Process declaration and add result to environment
                        printfn "-----"
                        printfn "Declaration %s:" v.FullName
                            
                        // A parameter on this level can be a single value or a tuple.
                        // TODO: Add actual tuple support.       
                        let rec buildTuple (ps : FSharpMemberOrFunctionOrValue list) =
                            match ps with
                            | [p] -> (p, fsharpTypeToSecType p.FullType)
                            //| p :: ps -> Tuple (fsharpTypeToSecType p.FullType, buildTuple ps)
                            | _ -> failwith "Tuple parameters not supported."
                                        
                        /// List of parameters and their reconstructed security type with generic levels.
                        let parameters =
                            parameters
                            |> List.map (fun p -> buildTuple p)

                        /// Environment with parameters added to Γ.
                        let env' =
                            {
                                env with
                                    Γ = 
                                        (
                                            parameters 
                                            |> List.map (fun (p,t) -> (p,t))
                                        )
                                        @ env.Γ
                            }

                        // Recursive helper function to turn parameters into nested function types.
                        let rec reconstructParameters = function
                        | [] -> TreeTraversal.traverse env' e
                        | (_,t) :: ps ->
                            let (c, t') = reconstructParameters ps
                            let l = freshLevel ()
                            // Put reconstructed type in front of return type.
                            // Leq constraint realizes (pc ⊑ pc') from rule APPLY,
                            // which unfortunately has to be repeated here.
                            (Leq (env'.pc, l) :: c,Fun (t, l, t'))

                        /// Get full type signature by extending inferred return-type signature with
                        /// reconstructed parameter types.
                        let (constraints', secType') = reconstructParameters parameters

                        // Ensure user-supplied type signature has the same structure
                        let (extendedSecType, compareConstraints) = match userSecType with
                        | Some (UserSupplied ut) ->
                            // Fails with exception when structure does not match.
                            let constraints = compareSecTypes (secType', ut)

                            // Test passed, use user-supplied type from now on.
                            (ut, constraints)
                        // No user-supplied type defined. Use inferred type.
                        | None -> (secType', [])

                        // Concatenate constraints
                        let constraints' = constraints' @ compareConstraints

                        // Show typing result for this function/value.
                        printfn " Security type: %+A" extendedSecType
                        printfn " Constraints: "
                        constraints' |> List.iter (fun c -> printfn "  %+A" c)

                        // Solve constraints
                        let s = solve constraints'
                        let (CSecType (_, resolvedSecType)) = subst s extendedSecType

                        printfn " Substitution: %+A" s
                        printfn " Final type: %+A" resolvedSecType

                        // Return updated list of functions/values and constraints.
                        // Return extended environment
                        {env with Γ = (v, resolvedSecType) :: env.Γ}

            // Initialization actions are ignored.
            | FSharpImplementationFileDeclaration.InitAction(_) -> env

        /// Processes all declarations of this file recursively
        decls |> List.fold (processDeclaration false) env


    /// Performs security type check using the specified project file.
    let processProjectFile projectFile =
        let projOptions = checker.GetProjectOptionsFromCommandLineArgs(projectFile, [|"..\\Simple\\Program.fs"|])

        /// Compilation result of F# Compiler Services.
        let results =
            checker.ParseAndCheckProject(projOptions)
            |> Async.RunSynchronously

        // Start processing the result obtained from F# Compiler Services.

        // Process all files in order.
        results.AssemblyContents.ImplementationFiles
        |> List.map (fun f -> f.Declarations)
        |> List.fold processDeclarations defaultEnv // Start with empty environment.
        |> ignore
        ()