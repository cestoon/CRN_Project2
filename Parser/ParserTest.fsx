#r "nuget:FParsec";;
#load "Parser.fs";;

open Parser;;

open FParsec

let gcd = "crn={
    conc[b,32 ],
    conc[a,12 ],
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

let p1 = "crn={ conc[b, 32], conc[a, 12]}"

let discreteCounter = "crn={
 conc[c,2], conc[cInitial, 4],
 conc[one ,1], conc[zero ,0],
 step[{
 sub[c,one,cnext ],
 cmp[c,zero]
 }],
 step[{
 ifGT[{ ld[cnext ,c] }],
 ifLE[{ ld[ cInitial ,c] }]
 }]
};"

let fac =  "crn={
 conc[ f ,1], conc[one ,1], conc[ i , 5 ],
 step[{
 cmp[i,one ],
 mul[f , i , fnext ],
 sub[ i ,one, inext ]
 }],
 step[{
 ifGT[{
 ld[ inext , i ],
 ld[ fnext , f ]
 }]
 }]
};"

parseString gcd    
