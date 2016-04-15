namespace Jslinq

module TreeTraversal =
    open Types
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

    /// Looks up AST and security type for function or value.
    let tryLookupFunctionOrValue (env : Environment) (func : FSharpMemberOrFunctionOrValue) : SecType option =
        match env.Γ |> List.tryFind (fun (f,_) -> f.IsEffectivelySameAs func) with
        | Some (_, st) -> Some st
        | None -> None

    /// Traverse AST and collect constraints as well as final return type recursively.
    let rec traverse (env : Environment) (expr : FSharpExpr) : (Constraint list * SecType) =
        
        /// Distinguishes hosting language from quoted language.
        let quotation = env.Δ |> Option.isSome

        /// Helper to iterate over list of parameters, using rule APPLY repeatedly.
        // pc,Γ,M ⊦ e1 : t ->^pc' t'
        let rec apply (Fun (t, pc', t')) es =
            match es with
            // Called without arguments, so just return function type.
            // Happens for example in partial function application.
            | ([]) -> ([], Fun (t, pc', t'))

            // Base case: applying a function to one parameter.
            | ([e]) ->
                // Determine type of value to which e1 is applied (e2).
                // pc,Γ,M ⊦ e2 : t
                let (e2Constraints, e2t) = traverse env e

                let newConstraints =
                    Leq (env.pc, pc') :: // pc ⊑ pc'
                    compareSecTypes (t, e2t) // Type of e2 must match type of e1's parameter.

                (newConstraints @ e2Constraints, t')

            // Traverse through curried function applications.
            | (e :: es) ->
                // Recurse into other function applications and get result.
                let (c1, traversedType) = apply t' (es)

                // Apply top-level function, using recursively determined type.
                let (c2, topLevelType) = apply (Fun (t, pc', traversedType)) ([e])

                // Concatenate constraints, using top-level type as result.
                (c1 @ c2, topLevelType)


//        printfn "Visiting expression %s..." (expr.ToString().[0..10])

        let result = match expr with

        // Rule UNIT
        // Γ,M ⊦ () : unit
        | Const (:? unit,_) -> ([], Unit)

        // Rule CONST
        | Const _ -> ([], Base (freshLevel ()))

        // Rules FUN, FUNQ
        | Lambda (v, e) ->
            let t = fsharpTypeToSecType v.FullType
            let env = {env with Γ = (v, t) :: env.Γ}
            let (constraints, t') = traverse env e
            (constraints, Fun (t, env.pc (* Correct? *), t'))

            
        // Coerce does not affect security types
        | Coerce (_, e) -> traverse env e

        | Let ((v, definedExpr), remainingExpr) ->
            // Determine security type of defined expression.
            let (c1, definedSecType) = traverse env definedExpr

            // Add defined type to environment.
            let env = {env with Γ = (v, definedSecType) :: env.Γ }

            // Determine type of remaining expression. (let x = 1 in ...)
            let (c2, t) = traverse env remainingExpr

            printfn "--Let %s: %A" v.DisplayName definedSecType
            (c1 @ c2, t)

        // TODO: Check this with model
        | Sequential (e1, e2) -> 
            let (c1, _) = traverse env e1
            let (c2, t) = traverse env e2
            (c1 @ c2, t)
            

        // Rules IF, IF1 and IFQ
        | IfThenElse (condExpr,thenExpr,elseExpr) ->
            let elseZero =
                match elseExpr with
                | Call (_,f,_,_,_) -> f.DisplayName = "Zero"
                | _ -> false

            match elseZero with
            // Rules IF1, IFQ
            | true ->
                // Determine level of condition boolean
                let (c1, Base l) = traverse env condExpr

                // Determine type and level of then expression
                let (c2, List (t,l')) = traverse env thenExpr

                // Construct resulting type
                // M : (t list)^(l ⊔ l')
                (c1 @ c2, List (t, (Join (l, l'))))

            // Rule IF
            | false ->
                // Determine constraints and type of condition expression.
                // pc,Γ,M ⊦ e : bool^l
                let (c1, Base l) = traverse env condExpr

                // Update environment used for branches.
                // pc ⊔ l
                let env' = { env with pc = Join (env.pc, l)}

                // Determine constraints and types of both branches.
                let (c2, st1) = traverse env' thenExpr // e_1
                let (c3, st2) = traverse env' elseExpr // e_2
                
                // Create a joined type from both branches
                let st = joinTypes (st1, st2)

                // l ⊑ t 
                let newConstraints = levelLeqType l st

                // Return type of first branch. Must be equal to second branch when
                // constraints are fulfilled.
                (newConstraints @ c1 @ c2 @ c3, st)


        | Call (obj, f, _, fsharpType, es) ->

            match tryLookupFunctionOrValue env f with
            // Rule APPLY.
            | Some funType ->
                // Simply use apply rule.
                apply funType (es)
            
            | _ ->
                match (f.FullName, quotation) with
                   
                // Allow Seq.toList without much hassle
                | ("Microsoft.FSharp.Collections.Seq.toList", false) ->
                    traverse env es.Head

                // Simple List.map
                // TODO: Verify
                | ("Microsoft.FSharp.Collections.List.map", false) ->
                    let (c1, Fun (mapParamSecType,_, mapResultSecType)) = traverse env es.[0]
                    let (c2, List (listElementSecType,l)) = traverse env es.[1]

                    // Level of list element tied to mapping input.
                    let c3 = compareSecTypes (mapParamSecType, listElementSecType)

                    (c1 @ c2 @ c3, List (mapResultSecType, l))

                | ("Microsoft.FSharp.Collections.List.length", false) ->
                    let (c1, List (_,l)) = traverse env es.[0]
                    (c1, Base l)

                | ("Microsoft.FSharp.Collections.List.forall", false) ->
                    let (c1, Fun (forallParamSecType,_, Base l)) = traverse env es.[0]
                    let (c2, List (listElementSecType,_)) = traverse env es.[1]
                    let c3 = compareSecTypes (forallParamSecType, listElementSecType)
                    (c1 @ c2 @ c3, Base l)

                | ("Microsoft.FSharp.Collections.List.filter", false) ->
                    let (c1, Fun (filterParamSecType,_, Base l1)) = traverse env es.[0]
                    let (c2, List (listElementSecType,l2)) = traverse env es.[1]
                    let c3 = compareSecTypes (filterParamSecType, listElementSecType)
                    (c1 @ c2 @ c3, List(listElementSecType, Join(l1,l2)))

                // List sorting
                // Note: This implementation supports only base types as elements.
                | ("Microsoft.FSharp.Collections.List.sortBy", false) ->
                    let (c1, Fun (_,_, Base keyLevel)) = traverse env es.[0]
                    let (c2, List (elementType, listLevel)) = traverse env es.[1]

                    let elementLevel = getOuterLevel elementType

                    // Apply level of key to all elements, since order discloses information about key.
                    (c1 @ c2, List (setOuterLevel (Join (elementLevel, keyLevel)) elementType, listLevel))

                | ("Microsoft.FSharp.Collections.List.nth", false) ->
                    let (c1, List (listElementSecType, _)) = traverse env es.[0]
                    let (c2, Base l) = traverse env es.[1]
                    (c1 @ c2, setOuterLevel (Join (getOuterLevel listElementSecType, l)) listElementSecType)

                | ("Microsoft.FSharp.Collections.List.concat", false) ->
                    let (c, List (listElementSecType, _)) = traverse env es.[0]
                    (c, listElementSecType)

                | ("Microsoft.FSharp.Core.Operators.( |> )", false) ->
                    let (c1, funcType) = traverse env es.[1]
                    let (c2, t) = apply funcType [es.[0]]
                    (c1 @ c2, t)

                // Rule REF
                | ("Microsoft.FSharp.Core.Operators.ref", false) ->
                    // Determine type of referenced expression.
                    let (c1, t) = traverse env es.Head

                    // pc ⊑ t
                    let c2 = levelLeqType env.pc t

                    (c1 @ c2, Ref (t, env.pc))

                // Rule DEREF
                | ("Microsoft.FSharp.Core.Operators.( ! )", false) ->
                    let (c1, t) = traverse env es.Head
                    match t with
                    | Ref (t, l) ->

                        // l ⊑ t
                        let c2 = levelLeqType l t

                        (c1 @ c2, setOuterLevel (Join (getOuterLevel t, l)) t)

                    | _ -> failwith "Rule DEREF failed."

                // Rule ASSN
                | ("Microsoft.FSharp.Core.Operators.( := )", false) ->
                    let (c1, Ref (t, l)) = traverse env es.[0]
                    let (c2, t2) = traverse env es.[1]

                    // (pc ⊔ l) ⊑ t
                    let c3 = levelLeqType (Join (env.pc, l)) t

                    ((compareSecTypes (t, t2)) @ c1 @ c2 @ c3, Unit)

                // Rule FST
                | ("Microsoft.FSharp.Core.Operators.fst", false) ->
                    let (c, Tuple (t, _)) = traverse env es.[0]
                    (c, t)

                // Rule SND
                | ("Microsoft.FSharp.Core.Operators.snd", false) ->
                    let (c, Tuple (_, t)) = traverse env es.[0]
                    (c, t)

                // Rules ANTIQUOTE & RUN
                | ("Microsoft.FSharp.Core.ExtraTopLevelOperators.( ~% )", true) | ("Run", false) ->
                    let (constraints, t) = traverse env es.Head
                    match t with
                    | Expr t -> (constraints, t)
                    | _ -> failwith "Rule ANTIQUOTE failed."

                // Rule YIELDQ
                | ("Microsoft.FSharp.Linq.Yield", true) ->
                    let (constraints, t) = traverse env es.Head
                    (constraints, List (t, freshLevel ()) )

                // Rule FORQ
                | ("Microsoft.FSharp.Linq.For", true) ->
                    let baseLevel =
                        function
                        | UserSupplied (Base l) -> Some l
                        | _ -> None

                    let result = 
                        maybe {
                            let! tableSignature = 
                                fsharpType.Head.TypeDefinition.Attributes
                                |> SignatureParser.tryGetUserSecType ""

                            // H,Δ ⊦ M : (t list)^l
                            let! l =
                                tableSignature
                                |> baseLevel

                            let! (_,e) = (|Lambda|_|) es.[1]
                            let! (_,e) = (|Let|_|) e

                            let (constraints, t') = traverse env e

                            // Assumes that result is a list and joins levels.
                            // N : (t' list)^(l⊔l')
                            return (constraints, setOuterLevel (Join (l, (getOuterLevel t'))) t')
                        }
                    match result with
                    | Some t -> t
                    | None -> failwith "Failure in rule FORQ."

                
                | (_, _) ->
                    
                    // Determine if we have a database member or not.
                    // TODO: Make sure this can only be used inside quotations. Maybe re-organize nested pattern matches.
                    match SignatureParser.tryGetAttributeArguments<System.Data.Linq.Mapping.ColumnAttribute>(f.Attributes) with

                    // We have a database column, so get type directly from annotation.
                    // This resembles the Σ mapping from the model.
                    | Some _ ->
                        let result =
                            maybe {
                                let! secType = SignatureParser.tryGetUserSecType "" f.Attributes
                                match secType with
                                | UserSupplied (Base l) -> return Base l
                                | _ -> return! None
                            }

                        match result with
                        | Some t -> ([],t)
                        | None -> failwith (sprintf "Column %s has missing or invalid security annotation." f.DisplayName)

                    // Rule OP
                    | None ->
                        // Use rule OP on operators and ignored modules.
                        let condition = 
                            f.FullName.StartsWith "Microsoft.FSharp.Core.Operators" ||
                            // Must be directly in ignored module. Nesting not supported with this code.
                            Option.isSome (SignatureParser.tryGetAttributeArguments<Ignore>(f.LogicalEnclosingEntity.Attributes))

                        match condition with
                        | true ->
                            printfn "Using OP rule on %s" f.FullName

                            // Get types for all parameters.
                            let typings = es |> List.map (fun e -> traverse env e)

                            // Merge constraints
                            let constraints = typings |> List.map fst |> List.concat

                            // Determine levels.
                            let levels = typings |> List.map (snd >> getOuterLevel)

                            // Merge levels
                            let joinedLevel =
                                match levels with
                                | [] -> failwith "Error in rule OP: No levels determined."
                                | [l] -> l
                                | l :: ls -> ls |> List.fold (fun x y -> Join (x,y)) l 

                            // Get return type from list of inferred types or function description
                            let returnFsharpType =
                                match fsharpType.Length with
                                | 0 -> f.ReturnParameter.Type
                                | _ -> fsharpType |> List.rev |> List.head

                            // Create fresh level as return type, so that signature only contains variable.
                            let finalLevel = freshLevel ()
                            (Eq (finalLevel, joinedLevel) :: constraints, setOuterLevel finalLevel (fsharpTypeToSecType returnFsharpType))
                        | false -> failwith (sprintf "Direct call of %s not supported. Consider adding an annotated wrapper to the policy." f.FullName)


        // Rule VAR
        | Value v ->
            // Look-up in context
            match env.Γ |> List.tryFind (fun (s,_) -> s = v) with
            | Some (_,t) -> ([],t)
            | None -> failwith (sprintf "Variable %s not found in context" v.DisplayName)

        // Rule RECORD
        | NewRecord (t, es) ->
            // Field names need to be retrieved from t, values are retrieved from es.
            let fieldsAndSecTypes =
                es
                |> Seq.zip t.TypeDefinition.FSharpFields
                |> Seq.map (fun (f,e) -> (f, traverse env e))

            let constraints =
                fieldsAndSecTypes
                |> Seq.map (fun (_,(c,_)) -> c)
                |> Seq.toList
                |> List.concat

            let ts =
                fieldsAndSecTypes
                |> Seq.map (fun (f,(_,t)) -> (f.Name, t))
                |> Seq.toList

            (constraints, Rec ts)

        // Rule PROJECT
        | FSharpFieldGet (Some v, _, f) ->
            // Determine type signature (usually only a look-up).
            let (constraints, t) = traverse env v

            // Try to find field in security type signature and return its type.
            let result =
                maybe {
                    let! fields =
                        match t with
                        | Rec ts -> Some ts
                        | _ -> None

                    let! (_,t) =
                        fields
                        |> List.tryFind (fun (s,_) -> s = f.Name)

                    return t
                }

            match result with
            | Some t -> (constraints, t)
            | None -> failwith "Failure in rule PROJECT."

        | NewUnionCase (listType, separator, elements) ->
            // Make sure we actually have a list.
            match (listType.TypeDefinition.FullName, separator.DisplayName) with
            | ("Microsoft.FSharp.Collections.FSharpList`1", "::") ->
                // Convert nested elements into flat list

                let rec flatten =
                    function
                    | [left;right] ->
                        match right with
                        | NewUnionCase (_,_,es) -> left :: flatten es
                        | _ -> failwith "Error while flattening union case."
                    | _ -> []
                        
                let result =
                    flatten elements
                    |> List.map (fun e -> traverse env e)
                    // Check for identical structure should not be necessary,
                    // since this is ensured by F#'s type system.

                let constraints =
                    result
                    |> List.map fst
                    |> List.concat

                let joinedLevel =
                    result
                    |> List.map (snd >> getOuterLevel)
                    |> fun (l :: ls) -> List.fold (fun x y -> Join (x,y)) l ls

                let typeTemplate =
                    result
                    |> List.map snd
                    |> List.head

                (constraints, List (setOuterLevel joinedLevel typeTemplate, freshLevel ()))

            | _ -> failwith "Only non-empty F# lists are supported."

        // Rule PAIR.
        | NewTuple (_, es) ->
            let rec buildTuple (es : FSharpExpr list) =
                match es with
                | [] -> failwith "Error in rule PAIR."
                | e :: [] ->
                    traverse env e
                | e :: es ->
                    let (c1, et) = traverse env e
                    let (c2, est) = buildTuple es
                    (c1 @ c2, Tuple (et, est))

            buildTuple es

        | TupleGet (_, i, tuple) ->
            // Get tuple type
            let (c, tupleType) = traverse env tuple

            // First flatten tuple into list
            let rec flatten =
                function
                | Tuple (t1, t2) -> t1 :: flatten t2
                | t -> [t]

            let types = flatten tupleType

            (c, types.[i])

        // TODO: Restrict to use with query { } or make generic.
        // TODO: Duplicates Rule QUOTE in large parts. Unify this!
        | Application (e,_,es) ->

            // Shortcut to process query {} computation expressions.
            let tryAsQuery =
                let env = {env with Δ = Some []}
                maybe {
                    // Decompose boiler-plate expression for query {...}
                    let! (_, e) = (|Lambda|_|) e
                    let! (_,_,_,_,e) = (|Call|_|) e
                    let! e = (|Quote|_|) e.Head

                    // Determine type of embedded expression and return it
                    return traverse env e
                }    
                
            match tryAsQuery with
            | Some x -> x

            // Attempt failed, so process as normal function application.
            | None ->
                let (c1, et) = traverse env e
                let (c2, t) = apply et es
                (c1 @ c2, t)

        // Rule QUOTE
        | Quote e ->
            /// env + Δ.
            let env = {env with Δ = Some []}

            // Try to interpret it as <@ query { ... } @>
            // To make things easier, we allow only the case in the scenarios.
            let tryAsQuery =
                maybe {
                    // Decompose boiler-plate expression for query {...}
                    let! (e,_,_) = (|Application|_|) e
                    let! (_, e) = (|Lambda|_|) e
                    let! (_,_,_,_,e) = (|Call|_|) e
                    let! e = (|Quote|_|) e.Head

                    // Determine type of embedded expression and return it
                    let (constraints, t) = traverse env e
                    return (constraints, Expr t)
                }

            match tryAsQuery with
            | Some x -> x
            | None ->
                // Try to interpret expression directly, without expecting any structure.
                let (constraints, t) = traverse env e
                (constraints, Expr t)
          
        // Catch-all for unsupported expressions.
        | _ -> failwith (sprintf "Unmatched: %+A (%s:%i)" expr expr.Range.FileName expr.Range.StartLine)

        result