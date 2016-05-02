// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System.IO
open System.Net

let doWebRequest (url:string) methd transformer =
    async {
        let request = WebRequest.Create(url, Method = methd)
        use! response = request.AsyncGetResponse()
        return! transformer (response.GetResponseStream())
    }

let readStreamAsString (stream:Stream) =
    async {
        use streamReader = new StreamReader(stream)
        return! streamReader.ReadToEndAsync() |> Async.AwaitTask
    }


[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let request = doWebRequest "http://heise.de/index.html" "GET" readStreamAsString
    let response = request |> Async.RunSynchronously

    printfn "%s...%s" response.[0..40] response.[(response |> String.length)-40..]

    0 // return an integer exit code

