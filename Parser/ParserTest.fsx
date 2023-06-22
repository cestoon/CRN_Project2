#r "nuget:FParsec";;
#load "../AST/Library.fs";;
#load "Parser.fs";;

open Parser;;
open FParsec

let parseTest s =
    match parseString s with 
    | Success (result, _, _) -> printfn $"crn : {result}"
    | Failure (errorMsg, _, _) -> printfn "Parsing failed: %s" errorMsg
 
let p1 = "crn={ conc[b, 32], conc[a, 12],};"

// Euclid's GCD (Fig. 3)
let gcd= "crn={
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

// Discrete counter (Fig. 6)
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

// Factorial (Fig. 7)
let Factorial = "crn = {
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

// Division (Fig. 8)
let Division = "crn = {
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

// Integer square root (Fig. 9)
let IntegerSquareRoot = "crn = {
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

// Approximating Euler's number through infinite series (Fig. 10)
let ApproxEulerNum = "crn = {
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

// Approximating Pi constant through infinite series (Fig. 11)
let approPi = "crn = {
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

// Alternative way to subtract (Fig. 13)
let subtract = "crn = {
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

parseTest p1
parseTest gcd
parseTest DiscreteCounter
parseTest Factorial
parseTest Division
parseTest IntegerSquareRoot
parseTest ApproxEulerNum
parseTest approPi
parseTest subtract