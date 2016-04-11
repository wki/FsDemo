// Weitere Informationen zu F# unter "http://fsharp.org".
// Weitere Hilfe finden Sie im Projekt "F#-Lernprogramm".

open System.Text.RegularExpressions

// Wording
// Options: Liste mit Datenstruktur der gewählten Kommandozeilen Optionen
// Switch: Spezifikation eines einzelnen Kommandozeilen Schalters


// Kommandozeilen Optionen
type Options = {
    help:   bool
    dryrun: bool
    file:   string option
    rest:   string list
}

let defaultOptions = {
    help   = false
    dryrun = false
    file   = None
    rest   = []
}

// Definition der einzelnen Switches
type Switch = {
    switch: string
    help: string
    arg: bool
    func: Options -> string -> Options
} with 
    member this.switches = this.switch.Split([| '|' |]) |> Array.toList
    member this.chars = this.switches |> List.filter (fun s -> String.length s = 1)
    member this.words = this.switches |> List.filter (fun s -> String.length s > 1)
    member this.isChar opt = List.exists (fun c -> opt = "-" + c) this.chars
    member this.isWord opt = List.exists (fun w -> opt = "--" + w) this.words
    member this.isSwitch str = this.isChar(str) || this.isWord(str)
    member this.charOptions = List.map (fun c -> "-" + c ) this.chars
    member this.wordOptions = List.map (fun w -> "--" + w) this.words
    member this.options = this.charOptions @ this.wordOptions
    member this.helpText = (List.fold (fun opt s -> opt + " " + s) "" this.options) + " --- " + this.help   

let switches = [
    { switch = "h|help";   arg = false; help = "show this help";                   func = fun options _   -> { options with help = true } }
    { switch = "n|dryrun"; arg = false; help = "dryrun – tell what would happen";  func = fun options _   -> { options with dryrun = true } }
    { switch = "f|file";   arg = true;  help = "file to process";                  func = fun options arg -> { options with file = Some arg } }
]

// diverse gefilterte Listen
let switchesWithArg    = switches |> List.filter (fun s -> s.arg)
let switchesWithoutArg = switches |> List.filter (fun s -> not s.arg)
let singleCharSwitches = switches |> List.map (fun s -> s.chars)

// Hilfe Text
let helpText = List.fold (fun help s -> s + "\n" + help) "" (List.map (fun (s:Switch) -> s.helpText) switches)


// Regex parser active Pattern
let (|MatchRegex|_|) regex args =
    let str = List.head args
    let m = Regex(regex).Match(str)

    // printfn "match regex: %s against args: %A success: %A" regex args m.Success
    match m.Success with
    | true  -> Some ((List.tail [for x in m.Groups -> x.Value]) @ List.tail args)
    | false -> None

// einfacher Parser, der aus einem Array einen Options Record erzeugt
let parseCommandline argv =
    // "-h", "x", ... --> "-h", "-x", ...
    let asArgs matched =
        List.item 0 matched :: "-" + List.item 1 matched :: List.skip 2 matched
    
    let singleSwitchArgsRegex = "^(-[" + (String.concat "|" (switchesWithArg |> List.collect (fun s -> s.chars))) + "])(.+)$"
    let singleSwitchNoArgsRegex = "^(-[" + (String.concat "|" (switchesWithoutArg |> List.collect (fun s -> s.chars))) + "])(.+)$"

    // switch zu "-x" string abrufen. -> switch option
    let pickSwitch str = List.tryFind (fun (s: Switch) -> s.isSwitch str) switches
    
    // effizienteres Abtesten eines Switches
    let needsNoArgs (switch: Switch option): bool =
        match switch with
            | None -> false
            | _ -> not switch.Value.arg

    let needsArgs (switch: Switch option): bool =
        match switch with
            | None -> false
            | _ -> switch.Value.arg

    // recursive parse helper function
    let rec parse (args (*: string list *), options: Options) =
        // 1. Schritt: auf Verdacht switch suchen
        let switch =
            match args with
                | [] -> None
                | x :: restArgs -> pickSwitch x

        // 2. Schritt: erstes Kommandozeilen Argument auswerten
        match args with
        | [] -> (args, options)

        // split up -xXXX into -x XXX
        | MatchRegex singleSwitchArgsRegex matched -> parse(matched, options)

        // split up -xy into -x -y
        | MatchRegex singleSwitchNoArgsRegex matched -> parse(asArgs matched, options)

        | x :: restArgs when needsNoArgs switch ->
            parse(restArgs, switch.Value.func options "")
        
        | x :: arg :: restArgs when needsArgs switch ->
            parse(restArgs, switch.Value.func options arg)
        
        // einfach aber geht.
        // | ("-f" | "--file") :: file :: restArgs -> parse(restArgs, { options with file = Some file })

        | x :: restArgs -> parse(restArgs, { options with rest = options.rest @ [x] })

    // run internal parser and isolate options only    
    let _, options = parse (argv |> Array.toList, defaultOptions)
    options

[<EntryPoint>]
let main argv = 
    printfn "argv = %A" argv

    let options = parseCommandline argv
    printfn "Options = %A" options

    if (options.help) then
        // let scriptName = System.Diagnostics.Process.GetCurrentProcess().MainModule.ModuleName
        let scriptName = System.IO.Path.GetFileName(System.Environment.GetCommandLineArgs().[0])
        printfn "\nusage: %s [options]\n%s\n" scriptName helpText

    0 // Exitcode aus ganzen Zahlen zurückgeben
