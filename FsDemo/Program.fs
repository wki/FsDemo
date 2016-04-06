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
let (|MatchRegex|_|) regex args =
    let str = List.head args
    let m = Regex(regex).Match(str)
    match m.Success with
    | true -> printfn "matched"; Some ((List.tail [for x in m.Groups -> x.Value]) @ List.tail args)
    | false -> printfn "not matched"; None


// einfacher Parser, der aus einem Array einen Options Record erzeugt
let parseCommandline argv =
    // "-h", "x", ... --> "-h", "-x", ...
    let asArgs matched =
        [List.head matched; "-" + (matched |> List.tail |> List.head)] @ (matched |> List.tail |> List.tail)

    // recursive parse helper function
    let rec parse (args, options) =
        match args with
        | [] -> (args, options)

        // split up -xXXX into -x XXX
        | MatchRegex @"^(-[f])(.+)$" matched -> parse(matched, options)

        // split up -xy into -x -y
        | MatchRegex @"^(-[hn])(.+)$" matched -> parse(asArgs matched, options)

        | ("-h" | "--help" | "-?") :: restArgs -> parse(restArgs, { options with help = true } )

        | ("-n" | "--dryrun") :: restArgs -> parse(restArgs, { options with dryrun = true } )

        | ("-f" | "--file") :: file :: restArgs -> parse(restArgs, { options with file = Some file })

        | x :: restArgs -> parse(restArgs, { options with rest = options.rest @ [x] })

    // run internal parser and isolate options only    
    let _, options = parse (argv |> Array.toList, defaultOptions)
    options

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let options = parseCommandline argv
    printfn "Options = %A" options

    0 // Exitcode aus ganzen Zahlen zurückgeben
