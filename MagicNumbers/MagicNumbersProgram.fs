// calculare magic numbers with n digits and a sum of q

let magicNums q n =
    // return [ [d;d;d]; ... ] length n, sum q
    let rec findMagicnums q n =
        match n with
           | 0 -> []
           | 1 when q >= 0 && q <= 9 -> [[q]]
           | 1 -> []
           | 2 -> [1..9]
                  |> List.filter (fun digit -> 2 * digit - 1 = q)
                  |> List.map (fun digit -> [ digit; digit-1 ])

           | _ -> [1..9]
                  |> List.filter (fun digit -> 2 * digit - 1 <= q)
                  |> List.collect (fun digit ->
                      let remainingSum = q - (2*digit - 1)
                      let remainingDigits = n - 2
                      (findMagicnums remainingSum remainingDigits)
                      |> List.map (fun magic -> (digit :: magic) @ [ digit - 1 ]))

    let toNumber = List.fold (fun acc digit -> acc * 10 + digit) 0

    [1..n]
    |> List.collect (findMagicnums q)
    |> List.map toNumber

// helper to match an integer from a string
let (|Integer|_|) str =
    let (success, i) = System.Int32.TryParse(str)
    match success with
    | true -> Some(i)
    | false -> None

[<EntryPoint>]
let main argv =
    let calculateAndPrintMagicNums q n =
        magicNums q n
        |> List.iter (printf "%d ")
        printfn ""

    if (argv |> Array.length <> 2) then 
        printfn "need 2 integer arguments: sum q and length n"
    else
        match argv with
        | [| Integer(q); Integer(n) |] -> calculateAndPrintMagicNums q n
        | _ -> printfn "not all are integers"
     
    0 // return an integer exit code
