// #############################################
// # Authors: BingKun                          #
// # Contributor: BingKun                      #
// # Date: Jun 19th                            #
// # Last edit: June 19th                      #
// #############################################
module RxnTest

open NUnit.Framework
open RxnParser.RxnParser
open FParsec
open AST.CRNPP
open AST.Rxn
open FsCheck
open FsCheck.NUnit


// parser test
let test1 = "rxn[A+B, A+B+C, 1.0]"
let test2 = "rxn[A, a, 1.0]"
let test3: string = test1 + "," + test2

let RxnParseTest parseRxn s =
    match parseRxn s with
    | Success _ -> true
    | Failure _ -> false


[<TestCase>]
let ``Test for empty production`` () = 
    let test = "rxn[C, \"\", 1.0]"
    Assert.IsTrue(RxnParseTest parseRxnString test)

[<TestCase>]
let ``Test Parsing for different reaction speed`` () = 
    let test1 = "rxn[A+B, A+B+C, 2.0]"
    Assert.IsTrue(RxnParseTest parseRxnString test1)

[<TestCase>]
let ``Test Parsing for baisc reaction`` () = 
    Assert.IsTrue(RxnParseTest parseRxnString test1)

[<TestCase>]
let ``Test for lowercase`` () = 
    Assert.IsTrue(RxnParseTest parseRxnString test2)

[<TestCase>]
let ``Test for rxn combination`` () = 
    Assert.IsTrue(RxnParseTest parseRxnString test3)