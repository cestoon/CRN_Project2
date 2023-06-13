# Grammar verification

In this document we will test our grammar and syntax rules on all provided examples to show that the grammar we have made is compatible with all programs from the original article.

## GCD

See Fig 3. (a)

```
crn = {
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
};
```

```mermaid
graph TD;
crn-->rootl0-->conc0 & conc1 & stepl;
conc0-->spec0 & num0;
conc1-->spec1 & num1;

stepl --> step0 & step1;
step0-->coml0-->com0 & com1 & com2;
com0-->comp0-->ld0-->spec3 & spec4;
com1-->comp1-->ld1-->spec5 & spec6;
com2-->noncomp0-->cmp-->spec7 & spec8;

step1-->coml1-->com3 & com4;
com3-->cond0-->coml2-->com5-->comp2-->spec9 & spec10 & spec11;
com4-->cond1-->coml3-->com6-->comp3-->spec12 & spec13 & spec14;
```