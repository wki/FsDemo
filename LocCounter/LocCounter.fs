// count nonempty non-comment lines in C# source code
// parts are eliminated by regular expressions which might yield wrong results

open System.IO
open System.Text.RegularExpressions

let printLineCountOfFiles mask dir =
    let collectFilesLike mask dir = Directory.EnumerateFiles(dir, mask, SearchOption.AllDirectories)

    let isWanted (path: string) =
        path.Split [| Path.DirectorySeparatorChar |]
        |> Seq.filter (fun dir -> dir = "bin" || dir = "obj")
        |> Seq.isEmpty

    let calculateLineCount file =
        let removeBalancedComments text = Regex.Replace(text, @"/\*.*?\*/", "", RegexOptions.Singleline)
        let removeLineEndComments text = Regex.Replace(text, @"//.*$", "", RegexOptions.Multiline)
        let removeTrailingWhiteSpace text = Regex.Replace(text, @"\s+$","", RegexOptions.Multiline)
        let countNonBlankLines text = Regex.Matches(text, @"^.+$", RegexOptions.Multiline).Count
    
        file
        |> File.ReadAllText
        |> removeBalancedComments
        |> removeLineEndComments
        |> removeTrailingWhiteSpace
        |> countNonBlankLines
    
    let printFileLines file = printfn "%s - %d" file

    let printSummary = printfn "TOTAL: %d"

    let countLinesPrintAndReturn file =
        let nr = file |> calculateLineCount
        printFileLines file nr
        nr

    dir 
    |> collectFilesLike mask
    |> Seq.filter isWanted
    |> Seq.map countLinesPrintAndReturn
    |> Seq.sum
    |> printSummary


[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    argv.[0] |> printLineCountOfFiles "*.cs"

    0 // return an integer exit code
