namespace Jslinq

/// Parses a string as a security type signature.
module SignatureParser = 
    open FParsec
    open Types

    let ws = spaces
    let str_ws s = pstring s >>. ws

    let securityType =
        (pchar 'H' >>% Level.H) <|>
        (pchar 'L' >>% Level.L) <|>
        (pchar '\'' >>. many1Chars (asciiLetter <|> digit) .>> spaces |>> LVar)

    let securityAnnotation = pchar '^' >>. spaces >>. securityType <|> preturn (LVar "*")

    /// Parse using OperatorPrecedenceParser. Uses after-string parser to get security level
    /// for some infix operators.
    let opp = new OperatorPrecedenceParser<Term,Level,unit>()
    let term = opp.ExpressionParser

    let record =
        (pchar '{' .>> 
         spaces >>. 
         sepEndBy
            (
                many1Chars (asciiLetter <|> digit) .>> spaces .>> pchar ':' .>> spaces .>>. term
            ) 
            (
                spaces .>> pchar ';' .>> spaces
            ) .>> 
         spaces .>> 
         pchar '}') |>> Rec

    opp.TermParser <-
        (pchar '_' >>. (spaces >>. securityAnnotation) |>> Base) <|>
        (pstring "unit" >>. spaces >>% Unit) <|>
        (pstring "Expr<" >>. term .>> pchar '>' |>> Expr ) <|>
        record <|>
        (between (str_ws "(") (str_ws ")") term)
        .>> spaces

    let spacedSecLevel = spaces >>. securityAnnotation .>> spaces

    opp.AddOperator(InfixOperator("*", spaces >>% (LVar "*") (* Not used *), 2, Associativity.Right (* Is this correct? *), fun x y -> Tuple (x, y)))
    opp.AddOperator(InfixOperator("->", spacedSecLevel, 1, Associativity.Right (* Is this correct? *), (), fun l x y -> Fun (x, l, y)))
    opp.AddOperator(PostfixOperator("list", spacedSecLevel, 20, true, (), fun l x -> List (x, l)))
    opp.AddOperator(PostfixOperator("ref", spacedSecLevel, 20, true, (), fun l x -> Ref (x, l)))

    let completeTerm = spaces >>. term .>> eof          

    let parse s = 
        match run completeTerm s with
        | Success (result, _, _) -> result
        | Failure (error, _, _)  ->
            printfn "Error: %s" (error.ToString())
            raise (JslinqException "Malformed security type signature.")

    let test s =
        match run completeTerm s with
        | Success (result, _, _) -> printfn "Success: %+A" result
        | Failure (error, _, _)  -> printfn "Error: %s" (error.ToString())

    let rec testLoop () =
        printf "term> "
        let input = System.Console.ReadLine()
        printfn "%+A" (parse input)
        testLoop ()

    let tryGetAttributeArguments<'a>(attribs : System.Collections.Generic.IList<Microsoft.FSharp.Compiler.SourceCodeServices.FSharpAttribute>) =
        maybe {
            let attribs = attribs |> List.ofSeq
            let! a = attribs |> List.tryFind (fun a -> a.AttributeType.CompiledName = typeof<'a>.Name)
            return a.ConstructorArguments
        }

    /// Tries to retrieve the user-supplied security type signature.
    let tryGetUserSecType varPrefix (attribs : System.Collections.Generic.IList<Microsoft.FSharp.Compiler.SourceCodeServices.FSharpAttribute>) : UserSecType option =
        maybe {
            let! args = tryGetAttributeArguments<SecT>(attribs)
            let (_, o) = args.[0]
            let secTypeSig = o :?> string
            let ust = parse secTypeSig

            let mapping =
                function
                // Replace wildcards from parser "*" with freshLevels. Cannot be done during parsing,
                // since parser needs to be able to go back etc.
                | "*" -> freshLevel ()

                // Normal level variables are prefixed with a counter, so that they are unique.
                | s ->
                    let s' = varPrefix + s
                    LVar s'

            return UserSupplied (mapLVars mapping ust)
        }
