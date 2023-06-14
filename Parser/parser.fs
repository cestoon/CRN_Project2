module Parser

open FParsec
open AST.CRNPP

let token p = p .>> spaces
let symbol s = token (pstring s)

let pSpecies = many1Satisfy (fun c -> isLetter c || isDigit c) |>> Species

let pNumber = pfloat

let pConc =
    pipe2
        (pSpecies .>> symbol ",")
        pNumber
        (fun s n -> (s, n))

let pNonComposableS =
    between (symbol "cmp[") (symbol "]")
        (pipe2 pSpecies (pSpecies .>> symbol ",") (fun s1 s2 -> Cmp (s1, s2)))

let pComposableS: Parser<ComposableS, unit> = 
    choice [
        between (symbol "ld[") (symbol "]")
            (pipe2 pSpecies (pSpecies .>> symbol ",") (fun s1 s2 -> Ld (s1, s2)));
        between (symbol "add[") (symbol "]")
            (pipe3 pSpecies (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") (fun s1 s2 s3 -> Add (s1, s2, s3)));
        between (symbol "sub[") (symbol "]")
            (pipe3 pSpecies (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") (fun s1 s2 s3 -> Sub (s1, s2, s3)));
        between (symbol "mul[") (symbol "]")
            (pipe3 pSpecies (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") (fun s1 s2 s3 -> Mul (s1, s2, s3)));
        between (symbol "div[") (symbol "]")
            (pipe3 pSpecies (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") (fun s1 s2 s3 -> Div (s1, s2, s3)));
        between (symbol "sqrt[") (symbol "]")
            (pipe3 pSpecies (pSpecies .>> symbol ",") (pSpecies .>> symbol ",") (fun s1 s2 s3 -> Sqrt (s1, s2, s3)));
    ]

let (pCommand, pCommandRef) = createParserForwardedToRef<Command, unit>()
let (pCommandList, pCommandListRef) = createParserForwardedToRef<CommandList, unit>()
let (pConditionalS, pConditionalSRef) = createParserForwardedToRef<ConditionalS, unit>()
let (pRootList, pRootListRef) = createParserForwardedToRef<RootList, unit>()

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

    pCommandListRef := many1 pCommand

    pRootListRef := 
        choice [
            pipe2 (between (symbol "conc[") (symbol "],") (pConc .>> spaces))
                  pRootList
                  (fun conc rootList -> ConcS (conc, rootList));
            many1 pStep |>> StepList
        ]

    pConditionalSRef :=
        choice [
            between (symbol "ifGT[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfGT);
            between (symbol "ifGE[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfGE);
            between (symbol "ifEQ[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfEQ);
            between (symbol "ifLT[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfLT);
            between (symbol "ifLE[") (symbol "]") (between (symbol "{") (symbol "}") pCommandList |>> IfLE);
        ]

let pCrn : Parser<Crn, unit> =
    between (spaces >>. symbol "crn={") (symbol "};")
        (pRootList |>> fun rl -> Crn rl)

let parseString = run (pCrn  .>>  eof) 

let gcd = "crn = {
    conc[a,a0], 
    conc[b,b0], 
    step[{ 
        ld[a, atmp], 
        ld[b, btmp],
        cmp[a,b] 
    }], 
    step[{
        ifGT[{ sub[atmp,btmp,a] }],
        ifLT[{ sub[btmp,atmp,b] }] 
    }] 
};"

parseString gcd