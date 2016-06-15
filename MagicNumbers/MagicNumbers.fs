// calculare magic numbers with n digits and a sum of q

// return [ [d;d;d]; ... ] length n, sum q
let rec magicnums q n =
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
                  (magicnums remainingSum remainingDigits)
                  |> List.map (fun magic -> (digit :: magic) @ [ digit - 1 ]))

let magic q n =
    let toNumber = List.fold (fun acc digit -> acc * 10 + digit) 0

    [1..n]
    |> List.collect (magicnums q)
    |> List.map toNumber

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
