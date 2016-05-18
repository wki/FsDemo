// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System


// Datei mit Metadaten
type File = {
    name: string
    mtime: DateTime
    size: int
}

// erzeugtes Artifakt
type Artefact = {
    thumbnail: File   // typically JPEG
    view: File        // typically PDF
}

// Nachricht im Fehlerfall
type Message = string

// Zustände eines "Work Item"
type WorkItemState =
    | EmptyWorkItem
    | FilledWorkItem of string list
    | CheckedWorkItem of File list
    | RenderedWorkItem of File list * Artefact
    | IncompleteWorkItem of string list * Message
    | UploadedWorkItem

// Work Item mit Zustand
type WorkItem = {
    id: int
    state: WorkItemState
}

// Commands / Events
type Command =
    | Fill of string list
    | CheckSuccess of File list
    | CheckFailure of string
    | Render of Artefact
    | Upload

type Event =
    | Filled of string list
    | Checked of File list
    | Failed of string
    | Rendered of Artefact
    | Uploaded


// execute a command based on a state
// state * command -> event
let exec state command =
    match state, command with
    | EmptyWorkItem, Fill files              -> Filled(files)
    | FilledWorkItem _, CheckSuccess files   -> Checked(files)
    | FilledWorkItem _, CheckFailure message -> Failed(message)
    | CheckedWorkItem files, Render artefact -> Rendered(artefact)
    | RenderedWorkItem _, Upload             -> Uploaded
    | _ -> failwithf "command %A not valid in state %A" command state


// apply an event
// state * event -> state
let apply state event =
    match state,event with
    | _, Filled files                          -> FilledWorkItem(files)
    | _, Checked files                         -> CheckedWorkItem(files)
    | FilledWorkItem files, Failed message     -> IncompleteWorkItem(files, message)
    | CheckedWorkItem files, Rendered artefact -> RenderedWorkItem(files, artefact)
    | _, Uploaded                              -> UploadedWorkItem
    | _ -> failwith "invalid state"

(*
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
*)

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

(*
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
*)
         
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

