// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

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


// 1. Schritt: Eingabe besorgen
let receiveRequest input =
    Success(input, [])

// 2. Schritt: Validation
let validateInput request =
    match request with
    | Success(input, messages) ->
        if input = "" then
            Failure [NameMustNotBeBlank]
        else
            Success(input,[])
    | Failure messages -> Failure messages
    
// 3. Schritt: Ergebnis ausgeben
let returnMessage request =
    match request with
    | Success(input, messages) -> 
        ignore
    | Failure messages -> 
        printf "%A" messages
        ignore

// Verarbeitungs-Kette
let processInput request =
    receiveRequest
    >> validateInput
    >> returnMessage


[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let input : TwoTrack<string> = Success("input", [])

    let result = processInput input

    0 // return an integer exit code
