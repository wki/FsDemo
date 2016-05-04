﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open System.Net

let doWebRequest (url:string) meth transformer =
    async {
        let request = WebRequest.Create(url, Method = meth)
        use! response = request.AsyncGetResponse()
        printfn "Thread: %d" System.Threading.Thread.CurrentThread.ManagedThreadId
        return! transformer (response.GetResponseStream())
    }

let readStreamAsString (stream:Stream) =
    async {
        use streamReader = new StreamReader(stream)
        return! streamReader.ReadToEndAsync() |> Async.AwaitTask
    }

module Computation =
    let dobind name f rest =
        printfn "Bind: name=%A f=%A rest=%A" name f rest
        rest

    let doret f =
        printfn "Return: f=%A" f
        f

    let doresult() =
        printfn "Zero:"
        49

    type ComputationBuilder() =
        member x.Bind((name,f), rest) = dobind name f rest
        member x.Return(f) = doret f
        member x.Zero() = doresult()



[<EntryPoint>]
let main argv = 
    printfn "%A" argv

//    printfn "Thread: %d" System.Threading.Thread.CurrentThread.ManagedThreadId
//
//    let request = doWebRequest "http://heise.de/index.html" "GET" readStreamAsString
//    let response = request |> Async.RunSynchronously
//
//    printfn "%s...%s" response.[0..40] response.[(response |> String.length)-40..]

    let comp = new Computation.ComputationBuilder()

    let x = comp {
            let! x = "adsf",3
            49 |> ignore
        }

    printfn "x=%A" x

    printfn "Press [enter] to continue"
    Console.ReadLine() |> ignore;

    0 // return an integer exit code

