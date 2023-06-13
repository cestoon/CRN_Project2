module Parser
open FParsec
open AST.CRNPP

let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws

let normalChar = satisfy (fun c -> c <> ',' && c <> '\r' && c <> '\n' && c <> ' ' && c <> '[' && c <> ']' && c <> '{' && c <> '}')

let stringLiteral =  manyChars normalChar .>> ws

let toLd =
    // ld [’〈species〉‘,’〈species〉']
    str_ws "ld" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> Ld

let toAdd =
    // ‘add [’〈species〉‘,’〈species〉‘,’〈species〉‘]
    str_ws "add" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]"
    |>> function
    |(species1, species2), species3
        -> Add(species1, species2, species3)

let toSub =
    // sub [’〈species〉‘,’〈species〉‘,’〈species〉‘]
    str_ws "sub" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]"
    |>> function
    |(species1, species2), species3
        -> Sub(species1, species2, species3)

let toMul =
    // mul [’〈species〉‘,’〈species〉‘,’〈species〉‘]
    str_ws "mul" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]"
    |>> function
    |(species1, species2), species3
        -> Mul(species1, species2, species3)

let toDiv =
    // div [’〈species〉‘,’〈species〉‘,’〈species〉‘]
    str_ws "div" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]"
    |>> function
    |(species1, species2), species3
        -> Div(species1, species2, species3)

let toSqrt =
    // sqrt [’〈species〉‘,’〈species〉‘]
    str_ws "sqrt" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> Sqrt

let toCmp =
    // ‘cmp [’〈species〉‘,’〈species〉‘]
    str_ws "cmp" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> Cmp

let composableCommand, (composableCommandRef: Parser<ComposableS,unit> ref) = createParserForwardedToRef()
let anyCommand, (anyCommandRef: Parser<Command,unit> ref) = createParserForwardedToRef()

let toIfGT =
    // ‘ifGT [’〈CommandSList〉‘]
    str_ws "ifGT" >>. between (str_ws "[{") (str_ws "}]") (sepBy (composableCommand .>> ws) (str_ws ",")) |>> IfGT

let toIfGE =
    // ‘ifGE [’〈CommandSList〉‘]
    str_ws "ifGE" >>. between (str_ws "[{") (str_ws "}]") (sepBy (composableCommand .>> ws) (str_ws ",")) |>> IfGE

let toIfEQ =
    // ifEQ [’〈CommandSList〉‘]
    str_ws "ifEQ" >>. between (str_ws "[{") (str_ws "}]") (sepBy (composableCommand .>> ws) (str_ws ",")) |>> IfEQ

let toIfLT =
    // ifLT [’〈CommandSList〉‘]
    str_ws "ifLT" >>. between (str_ws "[{") (str_ws "}]") (sepBy (composableCommand .>> ws) (str_ws ",")) |>> IfLT

let toIfLE =
    // ifLT [’〈CommandSList〉‘]
    str_ws "ifLE" >>. between (str_ws "[{") (str_ws "}]") (sepBy (composableCommand .>> ws) (str_ws ",")) |>> IfLE

do composableCommandRef := choice [
    toLd
    toAdd
    toSub
    toMul
    toDiv
    toSqrt
]

do anyCommandRef := choice [
    toLd
    toAdd
    toSub
    toMul
    toDiv
    toSqrt
    toCmp
    toIfGT
    toIfGE
    toIfEQ
    toIfLT
    toIfLE
]

let block, blockref = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser

let toConc =
    // 〈ConcS 〉 ::= ‘conc[’〈species〉‘,’〈number 〉‘]
    str_ws "conc" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. float_ws .>> str_ws "]" |>> Conc

let toConcS = 
    sepBy toConc (str_ws ",") 
    |>> function
    |(conc1: Conc) :: (tail: Conc list)
        -> ConcS(conc1, tail)

let toStep =
    // 〈〈StepS 〉 ::= ‘step [’CommandSList‘]
    //〈CommandSList〉 ::= 〈CommandS 〉
    //| 〈CommandS 〉 ‘,’ 〈CommandSList〉
    str_ws "step" >>.  between (str_ws "[{") (str_ws "}]") (sepBy (anyCommand .>> ws) (str_ws ",")) |>> Step

do blockref := choice [
                        toConc
                        toStep
                        ]

let toCrn =
    //〈Crn〉 ::= ‘crn = {’〈RootSList〉‘}’
    //〈RootSList〉 ::= 〈RootS 〉
    //| 〈RootS 〉 ‘,’ 〈RootSList〉
    str_ws "crn" >>.  str_ws "=" >>.  between (str_ws "{") (str_ws "}") (sepBy (block .>> ws) (str_ws ",")) |>> Crn.Roots

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test toConc "conc[ABC,5]"
