module Parser
open FParsec
open AST.CrnTypes

let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws

let normalChar = satisfy (fun c -> c <> ',' && c <> '\r' && c <> '\n' && c <> ' ' && c <> '[' && c <> ']' && c <> '{' && c <> '}')

let stringLiteral =  manyChars normalChar .>> ws

let toLD =
    // ld [’〈species〉‘,’〈species〉']
    str_ws "ld" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> CommandS.LD

let toADD =
    // ‘add [’〈species〉‘,’〈species〉‘,’〈species〉‘]
    str_ws "add" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> CommandS.ADD

let toSUB =
    // sub [’〈species〉‘,’〈species〉‘,’〈species〉‘]
    str_ws "sub" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> CommandS.SUB

let toMUL =
    // mul [’〈species〉‘,’〈species〉‘,’〈species〉‘]
    str_ws "mul" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> CommandS.MUL

let toDIV =
    // div [’〈species〉‘,’〈species〉‘,’〈species〉‘]
    str_ws "div" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> CommandS.DIV

let toSQRT =
    // sqrt [’〈species〉‘,’〈species〉‘]
    str_ws "sqrt" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> CommandS.SQRT

let toCMP =
    // ‘cmp [’〈species〉‘,’〈species〉‘]
    str_ws "cmp" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. stringLiteral .>> str_ws "]" |>> CommandS.CMP

let command, commandref = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser

let toifGT =
    // ‘ifGT [’〈CommandSList〉‘]
    str_ws "ifGT" >>. between (str_ws "[{") (str_ws "}]") (sepBy (command .>> ws) (str_ws ",")) |>> CommandS.IFGT

let toifGE =
    // ‘ifGE [’〈CommandSList〉‘]
    str_ws "ifGE" >>. between (str_ws "[{") (str_ws "}]") (sepBy (command .>> ws) (str_ws ",")) |>> CommandS.IFGE

let toifEQ =
    // ifEQ [’〈CommandSList〉‘]
    str_ws "ifEQ" >>. between (str_ws "[{") (str_ws "}]") (sepBy (command .>> ws) (str_ws ",")) |>> CommandS.IFEQ

let toifLT =
    // ifLT [’〈CommandSList〉‘]
    str_ws "ifLT" >>. between (str_ws "[{") (str_ws "}]") (sepBy (command .>> ws) (str_ws ",")) |>> CommandS.IFLT

let toifLE =
    // ifLT [’〈CommandSList〉‘]
    str_ws "ifLE" >>. between (str_ws "[{") (str_ws "}]") (sepBy (command .>> ws) (str_ws ",")) |>> CommandS.IFLE

do commandref := choice [
                        toLD
                        toADD
                        toSUB
                        toMUL
                        toDIV
                        toSQRT
                        toCMP
                        toifGT
                        toifGE
                        toifEQ
                        toifLT
                        toifLE
                        ]

let block, blockref = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser

let toConcS =
    // 〈ConcS 〉 ::= ‘conc[’〈species〉‘,’〈number 〉‘]
    str_ws "conc" >>. str_ws "[" >>. stringLiteral .>> str_ws "," .>>. float_ws .>> str_ws "]" |>> RootS.Conc

let toStepS =
    // 〈〈StepS 〉 ::= ‘step [’CommandSList‘]
    //〈CommandSList〉 ::= 〈CommandS 〉
    //| 〈CommandS 〉 ‘,’ 〈CommandSList〉
    str_ws "step" >>.  between (str_ws "[{") (str_ws "}]") (sepBy (command .>> ws) (str_ws ",")) |>> RootS.Step

do blockref := choice [
                        toStepS
                        toConcS
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

test toConcS "conc[ABC,5]"
