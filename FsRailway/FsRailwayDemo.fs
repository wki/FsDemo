// see: https://fsharpforfunandprofit.com/rop


// mögliche Nachrichten
type Message =
    | NameMustNotBeBlank
    | EmailMustNotBeBlank
    | EmailNotValid of string

// Ergebnis jedes railway Funktionsaufrufs
type TwoTrack<'TEntity> =
    | Success of 'TEntity * Message list
    | Failure of Message list


// 1. Schritt: Eingabe (string) verpacken
let receiveRequest input =
    printfn "receiveInput input = %s" input
    Success(input, [])

// 2. Schritt: Validation TwoTrack<string> -> TwoTrack<string>
let validateInput request =
    printfn "validateInput request = %A" request
    match request with
    | Success(input, messages) ->
        if input = "" then
            Failure [NameMustNotBeBlank]
        else
            Success(input,[])
    | Failure messages -> Failure messages
    
// 3. Schritt: Ergebnis ausgeben TwoTrack<string> -> unit
let showMessage request =
    printfn "showMessage request = %A" request
    match request with
    | Success(input, messages) ->
        printfn "Successful. Input: %s" input
    | Failure messages -> 
        printfn "Error. Messages: %A" messages

// Verarbeitungs-Kette
let processInput =
    receiveRequest
    >> validateInput
    >> showMessage


[<EntryPoint>]
let main argv = 
    printfn "Argv: %A" argv

    processInput "input"
    printfn "Done."

    0 // return an integer exit code
