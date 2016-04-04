// Weitere Informationen zu F# unter "http://fsharp.org".
// Weitere Hilfe finden Sie im Projekt "F#-Lernprogramm".

// Kommandozeilen Optionen
type Options = {
    help: bool
    file: string option
    rest: string list
}

let defaultOptions = { help = false; file = None; rest = [] }

// einfacher Parser, der aus einem Array einen Options Record erzeugt
let parseCommandline argv =
    let rec parse (args: string list, options: Options) =
        match args with
        | [] ->
            printfn "empty list"
            (args, options)

        | ("-h" | "--help" | "-?") :: restArgs ->
            printfn "help"
            parse(restArgs, { options with help = true } )

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
