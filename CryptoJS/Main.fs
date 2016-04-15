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
