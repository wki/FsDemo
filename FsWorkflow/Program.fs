// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

type Kind = Inside | Cover
type Region = Europe | America | Asia
type Season = FW | SS
type Year = int


// ------ Zustände eines Hangtags

// Anfangszustand
type EmptyBriefing = {
    createdAt: DateTime
    createdBy: string
}

// ausgefuelltes Briefing
type FilledBriefing = {
    createdAt: DateTime
    createdBy: string
    briefing: string
    kind: Kind
}

type UnapprovedBriefing = FilledBriefing

type ApprovedBriefing = {
    createdAt: DateTime
    createdBy: string
    briefing: string
    kind: Kind
    approvedAt: DateTime
    approvedBy: string
}

(*
// technical information of a Hangtag
type TechnicalInfo = {
    refNo: string
    name: string
    season: Season
    year: Year
}

type RenderInfo = {
    thumbnail: string
    lowresPdf: string
    hiresPdf: string
    nrPages: int
}

type Language = DE | EN | FR
type TextLine = Language * string
type TextContent = TextLine list

type Content = {
    id: int
    kind: Kind
    region: Region
    technicalInfo: TechnicalInfo option
    renderInfo: RenderInfo option
    textContent: TextContent list
}
*)

type Hangtag =
    | EmptyBriefing of EmptyBriefing
    | FilledBriefing of FilledBriefing
    | UnapprovedBriefing of UnapprovedBriefing
    | ApprovedBriefing of ApprovedBriefing


// ------- Funktionen zum Behandeln von Hangtags
let createHangtag person =
    EmptyBriefing { createdAt = DateTime.Now; createdBy = person }

let fillBriefing h t k =
    match h with
    | EmptyBriefing e ->  FilledBriefing { createdAt = e.createdAt; createdBy = e.createdBy; briefing = t; kind = k }
    | FilledBriefing f -> FilledBriefing { f with briefing = t; kind = k }
    | _ -> failwith "not an empty briefing, cannot fill"


// ------- Methoden auf die Zustände legen
type EmptyBriefing with
    member this.Fill = fillBriefing

           
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

