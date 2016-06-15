// simple regex based CSV parser
// Strategy:
//   - split text into lines
//   - recursively consume junks of a line

open System
open System.Text.RegularExpressions

let csvText = """Name;Arbeitsort;"bla fasel"
Peter;Köln;dort

Stefan;"Köln;Homeoffice";;foo;
Wolfgang;Erlangen Keller; "foo furz"
"""

// let separator = ";"
// let quote = "\""

let parseCsv (csv:string) =
    // helper function to assist the active patterns below
    let matchRegex regex text =
        let m = Regex.Match(text, regex)

        printfn "match '%s' with regex '%s': %A" text regex m.Success

        match m.Success with
        | true  -> Some(m.Groups.[1].Value, text.Substring(m.Groups.[0].Length))
        | false -> None
    
    // the two kinds of CSV fields as active patterns
    let (|UnquotedField|_|) = matchRegex @"^\s*([^\"";]*)\s*(;|$)"
    let (|QuotedField|_|)   = matchRegex @"^\s*""([^\""]*)""\s*(;|$)"

    // outer function returls list of values
    let splitValues line =
        // recursive helper function returns reverse list of values
        let rec split line matched =
            match line with
            | ""                        -> matched
            | UnquotedField(field,rest)
            | QuotedField(field,rest)   -> split rest (field :: matched)
            | _                         -> "error" :: matched

        split line []
        |> List.rev

    // split a string into individual lines
    let splitLines (text:string) =
        text.Split([|"\r\n"; "\r"; "\n"|], StringSplitOptions.RemoveEmptyEntries) 
        |> Array.toList

    printfn "CSV: %A" (csv |> splitLines |> List.map splitValues)



[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    csvText |> parseCsv

    printfn "press [enter] to continue"
    Console.ReadLine() |> ignore

    0 // return an integer exit code
