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

namespace JsHashing

open WebSharper.InterfaceGenerator

module Res =
    let CryptoJS_MD5 = 
        (Resource "CryptoJS_MD5" "http://crypto-js.googlecode.com/svn/tags/3.1.2/build/rollups/md5.js").AssemblyWide()

module Definition =

     let CryptoJS =
        Class "CryptoJS" |+> Static [Method "MD5" T<string -> string>]

     let Assembly =
        Assembly [
            Namespace "WebSharper.CryptoJS.Resources" [Res.CryptoJS_MD5]
            Namespace "WebSharper" [ CryptoJS ]
        ]

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
