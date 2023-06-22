// #############################################
// # Authors: BingKun                          #
// # Contributor: BingKun                      #
// # Date: Jun 12th                            #
// # Last edit: June 19th                      #
// #############################################
namespace RxnParser

open FParsec
open AST.CRNPP
open AST.Rxn

module RxnParser =
    let token p = p .>> spaces
    let symbol s = token (pstring s)

    let pSpecies = many1Satisfy (fun c -> isLetter c || isDigit c) |>> Species

    let pExpr: Parser<Expr, unit> =
        sepBy1 pSpecies (symbol "+")

    let pNumber: Parser<decimal,unit> = choice [
            pfloat |>> decimal
            pint32 |>> fun n -> decimal n
        ]

    let pEmptyString: Parser<Species list,unit> = (pstring "\"\"" <|> pstring "''") |>> fun _ -> []

    let pRxn: Parser<Rxn, unit> =
        between 
            (symbol "rxn" .>>. symbol "[") (symbol "]")
            (pipe3 
                pExpr 
                ((symbol "," .>> spaces) >>. (pExpr <|> pEmptyString)) 
                ((symbol "," .>> spaces) >>. pNumber) 
                (fun rs ps k -> Rxn (rs, ps, k)))


    let pCrn: Parser<Rxn list, unit> =
        sepBy1 pRxn (symbol ",")
        
    let parseRxnString = run (pCrn .>> eof)