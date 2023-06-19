#r "nuget:FParsec";;
#load "../../AST/Library.fs";;
#load "../RxnParser.fs";;

open RxnParser.RxnParser;;
open FParsec


// parser test
let test = "rxn[C, \"\", 1.0]"
let test0 = "rxn[C, \'\', 1.0]"
let test1 = "rxn[A+B, A+B+C, 1.0]"
let test2 = "rxn[A, a, 1.0]"
let test3: string = test1 + "," + test2

let RxnParseTest parseRxnString s =
    match parseRxnString s with
    | Success (rxn, _, _) -> printfn "parser success: %A" rxn
    | Failure (errMsg, _, _) -> printfn "parser failed: %s" errMsg

// use parseTest func
RxnParseTest parseRxnString test
RxnParseTest parseRxnString test0
RxnParseTest parseRxnString test1
RxnParseTest parseRxnString test2
RxnParseTest parseRxnString test3
