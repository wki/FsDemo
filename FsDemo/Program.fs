// Weitere Informationen zu F# unter "http://fsharp.org".
// Weitere Hilfe finden Sie im Projekt "F#-Lernprogramm".

open System.Collections.Generic
open System.Text.RegularExpressions

// Kommandozeilen Optionen
type Options = {
    help: bool
    dryrun: bool
    file: string option
    rest: string list
}

let defaultOptions = { help = false; dryrun = false; file = None; rest = [] }

(* type Switch =
    | ShortSwitch of char * string * (Options->Options) // symbol:char, description: string, setter: Options -> Options
    | LongSwitch of string * string * (Options->Options) // name:string, description: string, setter: Options-> Options
    *)

type Switch = {
    switch: string
    help: string
    arg: bool
    func: Options -> string -> Options
}
    with 
        member this.switches = this.switch.Split([| '|' |]) |> Array.toList
        member this.chars = this.switches |> List.filter (fun s -> String.length s = 1)
        member this.words = this.switches |> List.filter (fun s -> String.length s > 1)
        member this.isChar opt = List.exists (fun c -> opt = "-" + c) this.chars
        member this.isWord opt = List.exists (fun w -> opt = "--" + w) this.words
        member this.isSwitch str = this.isChar(str) || this.isWord(str)

let switches = [
    { switch = "h|help";   arg = false; help = "show this help";  func = fun options arg -> { options with help = true } }
    { switch = "n|dryrun"; arg = false; help = "dryrun - show what would happen";  func = fun options arg -> { options with dryrun = true } }
    { switch = "f|file";   arg = true;  help = "file to process"; func = fun options arg -> { options with file = Some arg } }
]

let switchesWithArg = switches |> List.filter (fun s -> s.arg)
let switchesWithoutArg = switches |> List.filter (fun s -> not s.arg)

let singleCharSwitches = switches |> List.map (fun s -> s.chars)

// Regex parser function
let (|MatchRegex|_|) regex args =
    let str = List.head args
    let m = Regex(regex).Match(str)
    match m.Success with
    | true -> printfn "matched"; Some ((List.tail [for x in m.Groups -> x.Value]) @ List.tail args)
    | false -> printfn "not matched"; None

let (|MatchSwitchNoArgs|) (args : string list) =
    let str = List.head args
    List.tryFind (fun (s: Switch) -> s.isSwitch str) switches

// einfacher Parser, der aus einem Array einen Options Record erzeugt
let parseCommandline argv =
    // "-h", "x", ... --> "-h", "-x", ...
    let asArgs matched =
        List.item 0 matched :: "-" + List.item 1 matched :: List.skip 2 matched
        // [List.head matched; "-" + (matched |> List.tail |> List.head)] @ (matched |> List.tail |> List.tail)

    let singleSwitchArgsRegex = "^(-[" + (String.concat "" (switchesWithArg |> List.collect (fun s -> s.chars))) + "])(.+)$"
    let singleSwitchNoArgsRegex = "^(-[" + (String.concat "" (switchesWithoutArg |> List.collect (fun s -> s.chars))) + "])(.+)$"


    // recursive parse helper function
    let rec parse (args, options : Options) =
        match args with
        | [] -> (args, options)

        // split up -xXXX into -x XXX
        | MatchRegex singleSwitchArgsRegex matched -> parse(matched, options)

        // split up -xy into -x -y
        | MatchRegex singleSwitchNoArgsRegex matched -> parse(asArgs matched, options)

        // | MatchSwitchNoArgs matched -> parse(List.tail args, matched.Value.func options "")
        | opt :: restArgs when List.exists (fun (s: Switch) -> s.isSwitch(List.item 0 args)) switches -> parse(restArgs, options)

        // | ("-h" | "--help" | "-?") :: restArgs -> parse(restArgs, { options with help = true } )

        // | ("-n" | "--dryrun") :: restArgs -> parse(restArgs, { options with dryrun = true } )

        | ("-f" | "--file") :: file :: restArgs -> parse(restArgs, { options with file = Some file })

        | x :: restArgs -> parse(restArgs, { options with rest = options.rest @ [x] })

    // run internal parser and isolate options only    
    let _, options = parse (argv |> Array.toList, defaultOptions)
    options

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let options = parseCommandline (* [| "-hn"; "-fsdf" |] // *) argv
    printfn "Options = %A" options

    0 // Exitcode aus ganzen Zahlen zurückgeben
