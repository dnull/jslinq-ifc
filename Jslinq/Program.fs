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