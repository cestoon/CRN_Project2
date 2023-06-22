// #############################################
// # Authors: BingKun                          #
// # Contributor: BingKun                      #
// # Date: Jun 19th                            #
// # Last edit: June 19th                      #
// #############################################
module ParserTests

open NUnit.Framework
open FParsec
open CrnParser.CrnParser
open AST
open FsCheck
open FsCheck.NUnit

// this function is used to check if a given string is successfully parsed
let IsParseSuccess (s: string) =
        match parseCrnString s with
        | Success _ -> true
        | Failure _ -> false

// eg: check some basic parsing success
[<TestCase>]
let ``Test Basic Parsing Success`` () = 
    let testInput = "crn = {conc[a, 1], step[{ld[a, b]}]};"
    Assert.IsTrue(IsParseSuccess testInput)

// eg: check some basic parsing failure
[<TestCase>]
let ``Test Basic Parsing Failure``() =
    let testInput = "crn = {conc[a, x], step[{ld[a, b]}]};"
    Assert.IsFalse(IsParseSuccess testInput)

// case: only with conc
[<TestCase>]
let ``only with conc``() =
    let testInput = "crn={ conc[b, 32], conc[a, 12],};"
    Assert.IsTrue(IsParseSuccess testInput)

// case: Euclid's GCD (Fig. 3)
[<TestCase>]
let TestExampleGcd() =
    let gcd="crn={
                conc[a,32], conc[b,12],
                step[{
                    ld[a,atmp],
                    ld[b,btmp],
                    cmp[a,b]
                }],
                step[{
                    ifGT[{ sub[atmp,btmp,a] }],
                    ifLT[{ sub[btmp,atmp,b] }]
                }]
            };"
    Assert.IsTrue(IsParseSuccess gcd)

// case: Discrete counter (Fig. 6)
[<TestCase>]
let TestExampleDiscreteCounter() =
    let DiscreteCounter = "crn = {
                conc[c,10], conc[cInitial,3],
                conc[one,1], conc[zero,0],
                step[{
                    sub[c,one,cnext],
                    cmp[c,zero]
                }],
                step[{
                    ifGT[{ ld[cnext,c] }],
                    ifLE[{ ld[cInitial,c] }]
                }]
            };"
    Assert.IsTrue(IsParseSuccess DiscreteCounter)


// case: Factorial (Fig. 7)
[<TestCase>]
let Factorial() =
    let FactorialStr = "crn = {
                conc[f,1], conc[one,1], conc[i,5],
                step[{
                    cmp[i,one],
                    mul[f,i,fnext],
                    sub[i,one,inext]
                }],
                step[{
                    ifGT[{
                        ld[inext,i],
                        ld[fnext,f]
                    }]
                }]
            };"
    Assert.IsTrue(IsParseSuccess FactorialStr)
// case: Division (Fig. 8)
[<TestCase>]
let Division() =
    let DivisionStr = "crn = {
                conc[a,20], conc[b,3], conc[one,1],
                step[{
                    cmp[a,b]
                }],
                step[{
                    ifGE[{
                        sub[a,b,anext],
                        add[q,one,qnext]
                    }]
                }],
                step[{
                    ifGE[{
                        ld[anext,a],
                        ld[qnext,q]
                    }],
                    ifLT[{ ld[a,r] }]
                }]
            };"
    Assert.IsTrue(IsParseSuccess DivisionStr)

// case: Interger square root (Fig. 9)
[<TestCase>]
let IntegerSquareRoot() = 
    let IntegerSquareRootStr = "crn = {
                conc[one,1], conc[n,10],
                step[{
                    add[z,one,znext],
                    mul[znext,znext,zpow],
                    cmp[zpow,n]
                }],
                step[{
                    ifLT[{ ld[znext,z] }],
                    ifGE[{ ld[z,out] }]
                }]
            };"
    Assert.IsTrue(IsParseSuccess IntegerSquareRootStr)

// case: Approximating Euler's number through infinite series (Fig. 10)
[<TestCase>]
let ApproxEulerNum() =
    let ApproxEulerNumStr = "crn = {
        conc[e,1], conc[element,1],
        conc[divisor,1], conc[one,1],
        conc[divisorMultiplier,1],
        step[{
            div[element,divisor,elementNext],
            add[divisor,one,divisorNext],
            add[e,elementNext,eNext]
        }],
        step[{
            ld[elementNext,element],
            ld[divisorNext,divisor],
            ld[eNext,e]
        }]
    };"
    Assert.IsTrue(IsParseSuccess ApproxEulerNumStr)

// case: Approximating Pi constant through infinite series (Fig. 11)
[<TestCase>]
let approxPi() = 
    let approxPiStr = "crn = {
        conc[four,4], conc[divisor1,1],
        conc[divisor2,3], conc[pi,0],
        step[{
            div[four,divisor1,factor1],
            add[divisor1,four,divisor1Next],
            div[four,divisor2,factor2],
            add[divisor2,four,divisor2Next],
            sub[factor1,factor2,factor],
            add[pi,factor,piNext]
        }],
        step[{
            ld[divisor1Next,divisor1],
            ld[divisor2Next,divisor2],
            ld[piNext,pi]
        }]
    };"
    Assert.IsTrue(IsParseSuccess approxPiStr)

// case: Alternative way to subtract (Fig. 13)
let subtract() = 
    let subtractStr = "crn = {
        conc[a,0.2], conc[b,0.2],
        conc[one,1], conc[zero,0],
        step[{
            cmp[b,zero]
        }],
        step[{
            ifGE[{
                sub[a,one,anext],
                sub[b,one,bnext]
            }]
        }],
        step[{
            ifGE[{
                ld[anext,a],
                ld[bnext,b]
            }]
        }]
    };"
    Assert.IsTrue(IsParseSuccess subtractStr)