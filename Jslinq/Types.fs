//   Copyright 2016 Daniel Schoepe, Benjamin Liebe
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

/// Defines custom types for security levels.
/// Follows the structure given in the paper.
module Types =
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpx

    exception JslinqException of string

    /// Type synonym for level variable type
    type LV = string

    /// Security level.
    [<StructuralEquality;StructuralComparison>] 
    type Level =
        | L
        | H 
        | LVar of LV // 'a
        | Join of Level * Level // 'a ⊔ 'b
        
    /// Returns a fresh level variable.
    // Inspired by "Encapsulating Mutable State"
    // in http://en.wikibooks.org/wiki/F_Sharp_Programming/Mutable_Data
    let freshLevel =
        let counter = ref 0
        fun () ->
            let name = sprintf "l%i" !counter
            incr counter
            LVar name

    /// Defines the security signature of a function.
    type SecT(signature : string) =
        inherit System.Attribute()
        member this.SecuritySignature = signature

    /// Allows a module to be ignored.
    type Ignore() =
        inherit System.Attribute()

    /// Declares that a module contains the policy.
    type Policy() =
        inherit System.Attribute()

    /// Term for a (security) type signature.
    type Term =
        | Base of Level
        | Unit
        | Fun of Term * Level * Term
        | Ref of Term * Level
        | Tuple of Term * Term
        | Rec of (string * Term) list // maybe use set instead?
        | List of Term * Level
        | Expr of Term

    type SecType = Term

    
    type UserSecType = UserSupplied of SecType

    /// Constraints for security types.
    type Constraint =
        | Eq of Level * Level  // 'a = 'b
        | Leq of Level * Level // 'a ⊑ 'b

    type CSecType = CSecType of Constraint list * SecType

    type Environment =
        {
            /// Program counter security label.
            pc : Level;

            /// Maps constants, operators and databases to security types.
            Σ : unit list;

            /// Maps variable name to security types.
            Γ : (FSharpMemberOrFunctionOrValue * SecType) list;

            /// For quoted expressions: maps variable name to security types.
            Δ : (FSharpMemberOrFunctionOrValue * SecType) list option;
        }

    let defaultEnv =
        {
            pc = freshLevel ();
            Σ = [];
            Γ = [];
            Δ = None;

        }

    // Computation expressions from FSharpx
    let maybe = Option.MaybeBuilder()

    /// Traverses a security type, calling the visitor on each, merging results with the specified function.
    let rec traverseSecType (secType : SecType) (visit : SecType -> 'a) (merge : 'a -> 'a -> 'a) : 'a =
        match secType with
        | Unit | Base _ | Rec [] -> visit secType
        | Fun (t,_,t') -> merge (visit t) (visit t')
        | Ref (t,_) -> visit t
        | Tuple (t1,t2) -> merge (visit t1) (visit t2)
        | Rec ((_,t)::ts) -> merge (visit t) (visit (Rec ts))
        | Expr t -> visit t
        | List (t,_) -> visit t
    
    /// Determines outer level of a given type. Follows strictly the definition from paper,
    /// fresh level variables are introduced for types without ^l.
    let rec getOuterLevel =
        function
        | Base l
        | Ref (_,l)
        | List (_,l) -> l
        // All others get a fresh level variable
        | _ -> freshLevel ()
        
    /// Sets the outer level of the security type.
    let rec setOuterLevel l = 
        function
        | Base _ -> Base l
        | Ref (t,_) -> Ref (t,l)
        | List (t,_) -> List (t,l)
        // All others remain as they are.
        | t -> t

    /// Joins all levels that can be found in the security type.
    let joinLevels secType =
        let merge x y = Join (x,y)
        traverseSecType secType getOuterLevel merge
        
    /// Constructs a generic security type from a given F# type definition.
    let rec fsharpTypeToSecType (fsType : FSharpType) : SecType =

        if fsType.IsTupleType then
            let ts = fsType.GenericArguments |> Seq.toList

            let rec buildTuple ts =
                match ts with
                | [t] -> fsharpTypeToSecType t
                | t :: ts -> Tuple (fsharpTypeToSecType t, buildTuple ts)

            buildTuple ts

        else if fsType.TypeDefinition.IsFSharpRecord then
            let fields =
                fsType.TypeDefinition.FSharpFields
                |> Seq.map (fun f -> (f.Name, fsharpTypeToSecType f.FieldType))
                |> Seq.toList
            Rec fields            
        else
            match fsType.TypeDefinition.DisplayName with
            | "unit" -> Unit
            | "bool" | "int" | "string" | "float" | "Pagelet" | "Element" -> Base (freshLevel ())
            | "IQueryable" -> Expr (fsharpTypeToSecType fsType.GenericArguments.[0])
            | "list" -> List (fsharpTypeToSecType (fsType.GenericArguments.[0]), (freshLevel ()))
            | "ref" -> Ref (fsharpTypeToSecType (fsType.GenericArguments.[0]), (freshLevel ()))
            | _ -> failwith (sprintf "Failed to convert F# type %+A to security type." fsType)

    /// Substitutes the specified level variable with something else returned by a function.
    /// Function can be for example freshLevel or a function returning a constant.
    let rec mapLVars (f : string -> Level) (t : SecType) =
        let rec subst =
            function
            | LVar s -> f s
            | Join (l1, l2) -> Join (subst l1, subst l2)
            | l -> l

        match t with
        | Base l -> Base (subst l)
        | Fun (x,l,y) -> Fun(mapLVars f x, subst l, mapLVars f  y)
        | Ref (x,l) -> Ref(mapLVars f x, subst l)
        | Tuple(x,y) -> Tuple(mapLVars f x, mapLVars f y)
        | Rec es -> Rec (es |> List.map (fun (x,y) -> (x, mapLVars f y)))
        | List (x,l) -> List(mapLVars f x,subst l)
        | Expr x -> Expr (mapLVars f x)
        | Unit -> Unit

    /// Implements subtyping relation between levels and types
    let rec levelLeqType l secType : Constraint list =
        match secType with
        | Unit -> []
        | Tuple (t1, t2) -> [t1; t2] |> List.map (levelLeqType l) |> List.concat
        | Fun (_, pc, t) -> Leq (l, pc) :: levelLeqType l t
        | Expr t -> levelLeqType l t
        | Rec fs -> fs |> List.map (fun (_,t) -> levelLeqType l t) |> List.concat
        | Base l' -> [Leq (l, l')]

        | Base l'
        // List not contained in theory. Is this correct?
        | List (_,l')
        | Ref (_,l') -> [Leq (l, l')]

    // Takes two types and creates a new one where the corresponding levels are joined.
    // Required for the branches in IF.
    let rec joinTypes =
        function

        | (Unit, Unit) -> Unit

        | (Base a, Base b) -> Base (Join (a,b))

        | (Fun (p1,a,r1), Fun (p2,b,r2)) ->
            Fun (joinTypes (p1, p2), Join (a,b), joinTypes (r1,r2))

        | (Ref (t1,a), Ref (t2,b)) | (List (t1,a), List(t2,b)) ->
            Ref (joinTypes (t1, t2), Join (a,b))

        | (Tuple (l1,r1), Tuple (l2,r2)) ->
            Tuple (joinTypes (l1,l2), joinTypes (r1,r2))

        | (Rec f1s, Rec f2s) ->
            let fs =
                (List.zip f1s f2s)
                |> List.map (fun ((s,a),(_,b)) -> (s,joinTypes (a,b)))
            Rec fs

        | (Expr l, Expr r) -> Expr (joinTypes (l, r))

        | _ -> failwith "Types do not have the same structure."
        
    /// Traverses two security types (which are expected to have identical structure)
    /// and returns Eq constraints for all level annotations.
    /// This can be used to require that two terms have equal structure and level values.
    let rec compareSecTypes =
        function

        | (Unit, Unit) -> []

        | (Base a, Base b) -> [Eq (a, b)]

        | (Fun (p1,a,r1), Fun (p2,b,r2)) ->
            [Eq (a, b)] @
            compareSecTypes (p1, p2) @
            compareSecTypes (r1, r2)

        | (Ref (t1,a), Ref (t2,b)) | (List (t1,a), List(t2,b)) ->
            [Eq (a, b)] @
            compareSecTypes (t1, t2)

        | (Tuple (l1,r1), Tuple (l2,r2)) ->
            compareSecTypes (l1, l2) @
            compareSecTypes (r1, r2)

        | (Rec [], Rec []) -> []
        | (Rec ((_,l)::ls), Rec ((_,r)::rs)) ->
            compareSecTypes (l, r) @
            compareSecTypes (Rec ls, Rec rs) 

        | (Expr l, Expr r) -> compareSecTypes (l, r)

        | _ -> failwith "Types do not have the same structure."

        
    open Microsoft.FSharp.Reflection 
    let everywhere<'a,'b>(f:'a->'a, src:'b) =   // '
        let ft = typeof<'a>             // '
        let rec traverse (o:obj) =
            let ot = o.GetType()
            if (ft = ot || ft = ot.BaseType) then
            f (o :?> 'a) |> box    // '
            elif FSharpType.IsUnion(ot) then
                let info,vals = FSharpValue.GetUnionFields(o, ot)
                FSharpValue.MakeUnion(info, vals |> Array.map traverse)
            else 
                o
        traverse src :?> 'b       // '