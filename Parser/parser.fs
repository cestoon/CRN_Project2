module Parser

open FParsec
open System

// code example
// 1 crn = {
// 2 conc[a,a0],
// 3 conc[b,b0],
// 4 step [{
// 5 ld[a, atmp], 6 ld[b, btmp], 7 cmp[a,b]
// 8 }],
// 9 step [{
// 10 ifGT[{ sub[atmp,btmp,a] }], 11 ifLT[{ sub[btmp,atmp,b] }] 12 }]
// 13 };


// ----------- todo ----------
// speciesParser
// numberParser
// concParser
// comparisonParser
// addParser
// subParser
// mulParser
// divParser
// sqrtParser
// loadParser
// arithmeticParser
// commandListParser
//  geParser
//  gtParser
//  eqParser
//  ltParser
//  leParser
//  commandParser
//  conditionParser
// commandParser
// rootParser
// rootListParser

// For the lexical part
// Handling terminal symbols: identifiers, integers, parantheses and '-'
// skip blanks after a token


// For the Grammer part

// The above grammar is rewritten to an LL(1) grammar using techniques described in 
// the notes by Sestoft and Friis Larsen, for example

(*
   T    -> V | C | ( E )
   Eopt -> - T Eopt | epsilon 
   E    -> T Eopt

The definition of parser combinators follows this grammar closely
*)

let identifier: Parser<string, unit> = many1Satisfy2L isLetter isLetterOrDigit "identifier"

let add: Parser<ArithmeticS, unit> =
    between (pstring "add[") (pstring "]")
        (sepBy identifier (pstring ","))
        |>> Add

let arithmetic: Parser<ArithmeticS, unit> =
    choice [
        add
        sub
        mul
        div
    ]

let ifGT: Parser<ConditionalS, unit> =
    between (pstring "ifGT[{") (pstring "}]")
        (sepBy commandS (pstring ","))
        |>> IfGT

