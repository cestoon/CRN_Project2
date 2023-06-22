namespace Compiler

open AST.CRNPP
open AST.Rxn
open State
open Compiler.CrnRoot

module Lib =
    let compileClocks rxns env =
        let rec compileClocks' c1 rxns = function
            | [] -> rxns
            | [cn] -> 
                let rxn = ([cn; c1] => [c1; c1])
                rxn::rxns
            | cn::cn'::cs -> 
                let rxn = [cn; cn'] => [cn';cn']
                let rxns' = rxn::rxns
                compileClocks' c1 rxns' (cn'::cs)

        let clocks = Environment.getClockSpeciesInOrder env
        printfn $"ClockSpecies: {clocks}"
        let c1 = List.head clocks
        compileClocks' c1 rxns clocks


    let compile (Crn(rootlist)) = 
            let (rxns, env) = compileRootList Environment.Init.newUninitializedEnv rootlist
            let rxns' = compileClocks rxns env  |> List.distinct
            let env' = 
                env
                |> Environment.Init.concCmp
                |> Environment.Init.concClocks
            (rxns', env')