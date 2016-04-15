namespace Jslinq

module Program =
    [<EntryPoint>]
    //[<System.STAThread>] // http://stackoverflow.com/a/16614159
    let main argv = 
        match argv.Length with
        | 1 ->
            try
                TypeChecker.processProjectFile (argv.[0])
                printfn "[+] Security Type Check successfully completed."
            with
                | ex -> printfn "[-] Security Type Check failed: %s" (ex.Message)

        | _ -> printfn "Please specify project file (*.fsproj) as command line argument."
        0