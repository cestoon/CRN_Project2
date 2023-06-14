module Program

open FParsec

// this is a practice file for FParsec
let pfloat: Parser<float, unit> = pfloat

let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]" // [4,5,6]

// skip space
let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
