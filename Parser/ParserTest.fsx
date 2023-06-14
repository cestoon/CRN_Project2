#r "nuget:FParsec";;
#load "Parser.fs";;

open Parser;;

open FParsec


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