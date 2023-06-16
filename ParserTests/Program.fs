open FParsec
open AST.CrnTypes
open Parser
open TypeChecker.TypeChecker

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
            }"

    let aCrn = Roots ([Conc ("a", 8.0); Conc ("b", 88.0);
        Step [LD ("a", "atmp"); LD ("b", "btmp"); CMP ("a", "b")];
        Step [IFGT [SUB (("atmp", "btmp"), "a")]; IFLT [SUB (("btmp", "atmp"), "b")]]])
    printfn "%A" aCrn
    checkCrnType aCrn |>ignore

    0
