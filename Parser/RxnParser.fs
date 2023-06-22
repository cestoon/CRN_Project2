// #############################################
// # Authors: BingKun                          #
// # Contributor: BingKun                      #
// # Date: Jun 12th                            #
// # Last edit: June 19th                      #
// #############################################
namespace RxnParser

open FParsec
open AST
open AST.CRNPP
open AST.Rxn

module RxnParser =
    let token p = p .>> spaces
    let symbol s = token (pstring s)

    let pSpecies = many1Satisfy (fun c -> isLetter c || isDigit c) |>> Species

    let pExpr: Parser<Expr, unit> =
        sepBy1 pSpecies (symbol "+")

    let pNumber = choice [
            pfloat;
            pint32 |>> fun n -> float n
        ]

    let pEmptyString = (pstring "\"\"" <|> pstring "''") |>> fun _ -> []

    let pRxn: Parser<Rxn, unit> =
        between (symbol "rxn" .>>. symbol "[") (symbol "]")
            (pipe3 pExpr ((symbol "," .>> spaces) >>. (pExpr <|> pEmptyString)) ((symbol "," .>> spaces) >>. pNumber) (fun rs ps k -> Rxn (rs, ps, k)))


    let pCrn: Parser<Rxn list, unit> =
        sepBy1 pRxn (symbol ",")
        
    let parseRxnString = run (pCrn .>> eof)