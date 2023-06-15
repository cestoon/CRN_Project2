module Parser

open FParsec
open AST.CRNPP

let token p = p .>> spaces
let symbol s = token (pstring s)

let pSpecies = many1Satisfy (fun c -> isLetter c || isDigit c) |>> Species

let pNumber = choice [
        pfloat;
        pint32 |>> fun n -> float n
    ]

let pConc: Parser<Conc, unit> = 
    between (symbol "conc" .>>. symbol "[") (symbol "]") 
        (pipe2 (pSpecies .>> (symbol "," .>> spaces)) pNumber (fun sp n -> (sp, n)))

let pComposableS: Parser<ComposableS, unit> =
    choice [
        between (symbol "ld" .>>. symbol "[") (symbol "]") 
            (pipe2 (pSpecies .>> (symbol "," .>> spaces)) pSpecies (fun sp1 sp2 -> Ld(sp1, sp2)));
        between (symbol "add" .>>. symbol "[") (symbol "]")
            (pipe3 pSpecies ((symbol "," .>> spaces) >>. pSpecies) ((symbol "," .>> spaces) >>. pSpecies) (fun s1 s2 s3 -> Add (s1, s2, s3)));
        between (symbol "sub" .>>. symbol "[") (symbol "]")
            (pipe3 pSpecies ((symbol "," .>> spaces) >>. pSpecies) ((symbol "," .>> spaces) >>. pSpecies) (fun s1 s2 s3 -> Sub (s1, s2, s3)));
        between (symbol "mul" .>>. symbol "[") (symbol "]")
            (pipe3 pSpecies ((symbol "," .>> spaces) >>. pSpecies) ((symbol "," .>> spaces) >>. pSpecies) (fun s1 s2 s3 -> Mul (s1, s2, s3)));
        between (symbol "div" .>>. symbol "[") (symbol "]")
            (pipe3 pSpecies ((symbol "," .>> spaces) >>. pSpecies) ((symbol "," .>> spaces) >>. pSpecies) (fun s1 s2 s3 -> Div (s1, s2, s3)));
        between (symbol "sqrt" .>>. symbol "[") (symbol "]")
            (pipe2 (pSpecies .>> (symbol "," .>> spaces)) pSpecies (fun s1 s2 -> Sqrt (s1, s2)));
    ]

let pNonComposableS =
    between (symbol "cmp" .>>. symbol "[") (symbol "]")
        (pipe2 (pSpecies .>> (symbol "," .>> spaces)) pSpecies (fun s1 s2 -> Cmp (s1, s2)))


let (pCommand, pCommandRef) = createParserForwardedToRef<Command, unit>()
let (pCommandList, pCommandListRef) = createParserForwardedToRef<CommandList, unit>()
let (pConditionalS, pConditionalSRef) = createParserForwardedToRef<ConditionalS, unit>()
let (pRootList, pRootListRef) = createParserForwardedToRef<RootListS, unit>()

let pStep =
    between (symbol "step[{") (symbol "}]")
        (pCommandList)
    |>> Step

do
    pCommandRef :=
        choice [
            pComposableS |>> Composable;
            pNonComposableS |>> NonComposable;
            pConditionalS |>> Conditional;
        ]

    pCommandListRef := sepEndBy pCommand (symbol ",")

    let pConcList = sepEndBy pConc (symbol ",")
    let pSteps = sepEndBy1 pStep (symbol ",")

    // steps can be empty
    pRootListRef :=
        pipe2 pConcList (opt (pSteps .>> spaces)) (fun concs steps -> match steps with
                                                                    | Some s -> RootList (concs, s)
                                                                    | None -> RootList (concs, []))



    let conditionalParser: Parser<ConditionalS, unit> =
        choice [
            between (symbol "ifGT[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfGT);
            between (symbol "ifGE[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfGE);
            between (symbol "ifEQ[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfEQ);
            between (symbol "ifLT[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfLT);
            between (symbol "ifLE[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfLE);
        ]

    pConditionalSRef := conditionalParser

let pCrn : Parser<Crn, unit> =
    between (spaces >>. symbol "crn" .>>. spaces .>>. symbol "=" .>>. spaces .>>. symbol "{") (symbol "};")
        (pRootList |>> fun rl -> Crn rl)

let parseString = run (pCrn  .>>  eof) 
