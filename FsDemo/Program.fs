// Weitere Informationen zu F# unter "http://fsharp.org".
// Weitere Hilfe finden Sie im Projekt "F#-Lernprogramm".

open System.Text.RegularExpressions

// Kommandozeilen Optionen
type Options = {
    help: bool
    dryrun: bool
    file: string option
    rest: string list
}

let defaultOptions = { help = false; dryrun = false; file = None; rest = [] }

// Regex parser function
let (|MatchRegex|_|) (regex:string) (args: string list) =
    let str = List.head args

    let m = Regex(regex).Match(str)
    match m.Success with
    | true -> printfn "matched"; Some ((List.tail [for x in m.Groups -> x.Value]) @ List.tail args)
    | false -> printfn "not matched"; None


// einfacher Parser, der aus einem Array einen Options Record erzeugt
let parseCommandline argv =
    let rec parse (args, options) =
        match args with
        | [] ->
            printfn "empty list"
            (args, options)

        // split up -xXXX into -x XXX
        | MatchRegex @"^(-[f])(.+)$" matched ->
            printfn "Matched str: %A" matched
            parse(matched, options)

        // split up -xy into -x -y
        | MatchRegex @"^(-[hn])(.+)$" matched ->
            printfn "Matched opt: %A" matched
            parse([List.head matched; "-" + (matched |> List.tail |> List.head)] @ (matched |> List.tail |> List.tail), options)

        | ("-h" | "--help" | "-?") :: restArgs ->
            printfn "help"
            parse(restArgs, { options with help = true } )

        | ("-n" | "--dryrun") :: restArgs ->
            printfn "dryrun"
            parse(restArgs, { options with dryrun = true } )

        | ("-f" | "--file") :: file :: restArgs ->
            printfn "file: %s" file
            parse(restArgs, { options with file = Some file })

        | x :: restArgs ->
            printfn "unknown: %s" x
            parse(restArgs, { options with rest = options.rest @ [x] })

    // run internal parser and isolate options only    
    let _, options = parse (argv |> Array.toList, defaultOptions)
    options

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let options = parseCommandline argv
    printfn "Options = %A" options

    0 // Exitcode aus ganzen Zahlen zurückgeben
