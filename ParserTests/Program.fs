open FParsec
open Parser

[<EntryPoint>]
let main _ =

    test toCrn "crn = {
                conc[a, 8], conc[b,88],
                step[{
                    ld[a,atmp],
                    ld[b,btmp],
                    cmp[a,b]
                }],
                step[{
                    ifGT[{ sub[atmp,btmp,a] }],
                    ifLT[{ sub[btmp,atmp,b] }]
                }]
            }" |> printfn "%O"
    0
