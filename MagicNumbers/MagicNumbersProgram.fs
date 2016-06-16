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

    let toNumber = 
        let appendDigit acc digit = acc * 10 + digit 
        List.fold appendDigit 0

    [1..n]
    |> List.collect (findMagicnums q)
    |> List.map toNumber

[<EntryPoint>]
let main argv =
    let (|Integer|_|) str =
        let success, i = System.Int32.TryParse(str)
        match success with
        | true  -> Some i
        | false -> None

    let calculateAndPrintMagicNums q n =
        magicNums q n
        |> List.iter (printf "%d ")
        printfn ""

    match argv |> Array.toList with
    | "-h" :: _                -> printfn "please add 2 integers: Sum q and Nr Digits n" 
    | [Integer(q); Integer(n)] -> calculateAndPrintMagicNums q n
    | _                        -> printfn "wrong arguments. try -h"
     
    0 // return an integer exit code
