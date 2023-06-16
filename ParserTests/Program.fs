open FParsec
open AST.CRNPP
open Parser
open TypeChecker.TypeChecker

[<EntryPoint>]
let main _ =

    let test s =
        match parseString s with
        | Success (result, _, _) -> 
            printfn $"crn : {result}"
            result
        //| Failure (errorMsg, _, _) -> printfn "Parsing failed: %s" errorMsg


    let aCrn = test"crn={
                conc[a,32], conc[b,12],
                step[{
                    ld[a,atmp],
                    ld[b,btmp],
                    cmp[a,b]
                }],
                step[{
                    ifGT[{ sub[atmp,btmp,a] }],
                    ifLT[{ sub[btmp,atmp,b] }]
                }]
            };"
    printfn "%A" aCrn
    let ifok = checkCrn aCrn
    printfn "%A" ifok
    0
