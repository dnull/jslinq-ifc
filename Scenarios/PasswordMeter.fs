namespace Scenarios

open WebSharper

module PasswordMeter =

    open JsLib

    [<JavaScript>]
    let CheckPW (out : WebSharper.Html.Client.Element) (pw : string) =

        let check name exp =
            Div [
                Text (name + ": " + (if (JsOp.testRegExp exp pw) then "passed" else "failed"))
            ]

        let newContent =
            Div [
                check "Has at least 12 characters" ".{12,}"
                check "Contains small letters" "[a-z]"
                check "Contains capital letters" "[A-Z]"
                check "Contains digits" "\d"
                check "Contains special characters" "[^\w\s]"

                // Neutral image is allowed.
                Image [Src "http://placehold.it/100"]

                // Image with password in URL is rejected.
                //Image [Src ("http://placehold.it/100&text=" + pw)]
                
                // Because of low side-effects, image loading when branching on secret is not allowed.
                //Image [(if pw = "foo?" then Src "http://placehold.it/100&text=yes" else Src "http://placehold.it/100&text=no")]
            ]

        // Finally write new content in output Div
        SetContent out newContent
        

    [<JavaScript>]
    let Main () =
        let out = Div [Text "Foo"]
        let passwordIn = InputPW ()

        let handler () = CheckPW out (GetElementValue passwordIn)

        Div [
            passwordIn |>! OnKeyDown handler
            out
        ]