namespace Jslinq


module Solve =
    open Types
    
    type Substitution = { substs : (LV * Level) list ;
                        // remaining constraints:
                        remConstraints : Constraint list; }

    let emptySubst : Substitution = { substs = []; remConstraints = []; }

    let rec isConst (l : Level) : bool =
        match l with
            | L -> true
            | H -> true
            | LVar _ -> false
            | Join (l1, l2) -> isConst l1 && isConst l2

    let rec simplifyLevel (l : Level) : Level =
        match l with
            | L -> L
            | H -> H
            | Join (l1, l2) when simplifyLevel l1 = H || simplifyLevel l2 = H -> H
            | Join (l1, l2) when simplifyLevel l1 = L -> simplifyLevel l2
            | Join (l1, l2) when simplifyLevel l2 = L -> simplifyLevel l1
            | l -> l

    let rec levelVars (l : Level) : LV list =
        match l with
            | LVar lv -> [lv]
            | Join (l1, l2) -> levelVars l1 @ levelVars l2
            | _ -> []

    /// replaceLVarInLevel l1 lv l2 substitutes lv for l1 in l2
    let rec replaceLVarInLevel (l1 : Level) lv (l2 : Level) : Level =
        match l2 with
            | LVar lv' -> if lv = lv' then l1 else LVar lv'
            | L -> L
            | H -> H
            | Join (l2', l2'') -> Join (replaceLVarInLevel l1 lv l2', replaceLVarInLevel l1 lv l2'')

    let replaceLevelInConstraint (l : Level) (lv : LV) (c : Constraint) : Constraint =
        match c with
            | Eq (l1, l2) -> Eq (replaceLVarInLevel l lv l1, replaceLVarInLevel l lv l2)
            | Leq (l1, l2) -> Leq (replaceLVarInLevel l lv l1, replaceLVarInLevel l lv l2)

    let rec replaceLVarInSubst (lv : LV) (l : Level) (s : Substitution) : Substitution =
        { s with remConstraints = List.map (fun c -> replaceLevelInConstraint l lv c) s.remConstraints;
                 substs = List.map (fun (lv', l') -> (lv', replaceLVarInLevel l lv l')) s.substs }

    let replaceAllVars (vs : LV list) (l : Level) (s : Substitution) : Substitution =
        let s' = List.fold (fun s lv -> replaceLVarInSubst lv l s) s vs
        in { s' with substs = List.map (fun lv -> (lv, l)) vs @ s'.substs }

    let handleConstraint (s : Substitution) : Substitution =
        let s' = { s with remConstraints = match s.remConstraints with 
            | [] -> []
            | c :: cs -> cs }
        match s.remConstraints with
            | (Eq (l1, l2) :: cs) when isConst l1 && isConst l2 -> if simplifyLevel l1 <> simplifyLevel l2
                                                                   then failwith ("Constraints violated: " + s.ToString()) // double-check
                                                                   else { s with remConstraints = cs }
            | (Eq (l1, l2) :: cs) when l1 = L || l2 = L ->
                replaceAllVars (levelVars l2 @ levelVars l1) L s'
            | (Eq (LVar lv, l) :: cs) -> replaceAllVars [lv] l s'
            | (Eq (l, LVar lv) :: cs) -> replaceAllVars [lv] l s'
            | (Leq (l, H) :: cs) -> s'
            | (Leq (L, l) :: cs) -> s'
            | (Leq (l, L) :: cs) -> replaceAllVars (levelVars l) L s'
            | (c :: cs) -> { s with remConstraints = cs @ [c] }
            | [] -> s

    let rec solve' (s : Substitution) (num : int) : Substitution =
        let s' = everywhere (simplifyLevel, handleConstraint s)
        in if s.remConstraints.Length = s'.remConstraints.Length
           then if num > s.remConstraints.Length + 10
                then s'
                else solve' s' (num + 1) // repeat to try to find fixed point
           else solve' s' 0

    let solve (cs : Constraint list) : Substitution = solve' { substs = []; remConstraints = everywhere (simplifyLevel, cs) } 0

    let subst (s : Substitution) (t : SecType) : CSecType =
        let mapping (selector : LV) (replacement : Level) (x : LV) =
            if x = selector then replacement else LVar x

        CSecType (s.remConstraints, List.fold (fun t (lv, l) -> mapLVars (mapping lv l) t) t s.substs)
