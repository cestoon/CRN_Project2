// #############################################
// # Authors: Mads                             #
// # Contributor: Mads;                        #
// # Date: Jun 16th                            #
// # Last edit: June 16th                      #
// #############################################
namespace Compiler

open AST.CRNPP
open AST.Rxn
open State
open State.Environment
open Compiler

module CrnCommands =
    let compileComposable env = 
        let toRxn = function 
            | Ld(a, b) -> ([[a] => [a; b]], env)
            | Add(a, b, c) -> 
                let rxns = [ 
                    [a] => [a; c]
                    [b] => [b; c]
                    [c] => [] 
                ]
                (rxns, env)
            | Sub(a, b, c) -> 
                let (h, env') = getNewHelperSpecies env
                let rxns = [ 
                    [a]    => [a; c] 
                    [b]    => [b; h]
                    [c]    => []
                    [c; h] => []
                ]
                (rxns, env')
            | Mul(a, b, c) -> 
                let rxns = [
                    [a; b] => [a; b; c]
                    [c]    => []
                ]
                (rxns, env)
            | Div(a, b, c) ->
                let rxns = [
                    [a]    => [a; c]
                    [b; c] => [b]
                ]
                (rxns, env)
            | Sqrt(a, b) ->
                let rxns = [
                    [a]    =|1.0m|=> [a; b]
                    [b; b] =|0.5m|=> []
                ]
                (rxns, env)
        toRxn >> RxnSystem.addClock

    
    // Source: https://github.com/marko-vasic/crnPlusPlus/tree/master/packages/primitives.m
    // NormalizeToFlags[x_, y_] :=
    //     (
    //         Sequence @@ Flatten[
    //         {
    //             rxn[XgtyFlag + y, XltyFlag + y, 1],
    //             rxn[XltyFlag + ComparisonOffset, XgtyFlag + ComparisonOffset, 1],
    //             rxn[XltyFlag + x, XgtyFlag + x, 1],
    //        
    //             rxn[YgtxFlag + x, YltxFlag + x, 1],
    //             rxn[YltxFlag + ComparisonOffset, YgtxFlag + ComparisonOffset, 1],
    //             rxn[YltxFlag + y, YgtxFlag + y, 1]
    //         }]
    //     )
    //  (*Code here ommitted to bring these two parts close to each other*)
    // AM[x_, y_] :=
    //     (
    //         Module[ {b},
    //             Sequence @@ Flatten[
    //             {
    //                 rxn[x + y, y + b, 1],
    //                 rxn[b + y, y + y, 1],
    //                 rxn[y + x, x + b, 1],
    //                 rxn[b + x, x + x, 1]
    //             }]
    //         ]
    //     )
    //  (*Code here ommitted to bring these two parts close to each other*)
    //      AM[XgtyFlag, XltyFlag],
    //      AM[YgtxFlag, YltxFlag]
    let compileNonComposable env = function
        | Cmp(x, y) -> 
            let (aproxMajorClock, env') = getNewClock env
            let { Xgty = X_gty;  
                  Xlty = X_lty; 
                  Ygtx = Y_gtx; 
                  Yltx = Y_ltx; 
                  B = b; 
                  ComparisonOffset = offset } = env'.flagSpecies

            let mappingRxns =
                [   // Mapping Reaction (turn x & y into an appropriate amount of the flag species) 
                    [X_gty; y] => [X_lty; y]
                    [X_lty; offset] => [X_gty; offset]
                    [X_lty; x] => [X_gty; x] 

                    [Y_gtx; x] => [Y_ltx; x]
                    [Y_ltx; offset] => [Y_gtx; offset]  
                    [Y_ltx; y] => [Y_gtx; y] 

                ] |> List.map (addCatalyser env'.stepClock)
                    
            let aproxMajorityRxns =
                [   // Aproximate Majority Reactions 
                    [X_gty; X_lty] => [X_lty; b    ]
                    [b;     X_lty] => [X_lty; X_lty]
                    [X_lty; X_gty] => [X_gty; b    ]
                    [b;     X_gty] => [X_gty; X_gty]

                    [Y_gtx; Y_ltx] => [Y_ltx; b    ]
                    [b;     Y_ltx] => [Y_ltx; Y_ltx]
                    [Y_ltx; Y_gtx] => [Y_gtx; b    ]
                    [b;     Y_gtx] => [Y_gtx; Y_gtx] 
                ] |> List.map (addCatalyser aproxMajorClock)
            let rxns = mappingRxns @ aproxMajorityRxns
            (rxns, env')


    let rec compileConditional env cond = 
        let { Xgty = X_gty; Xlty = X_lty; Ygtx = Y_gtx; Yltx = Y_ltx } = env.flagSpecies
        let compileMapBody f = RxnSystem.compileMapMany (compileCommand) f env

        match cond with
        | IfGT(cs) -> cs |> compileMapBody (addCatalysers [X_gty; Y_ltx])
        | IfGE(cs) -> cs |> compileMapBody (addCatalyser X_gty)
        | IfEQ(cs) -> cs |> compileMapBody (addCatalysers [X_gty; Y_gtx])
        | IfLE(cs) -> cs |> compileMapBody (addCatalyser Y_gtx)
        | IfLT(cs) -> cs |> compileMapBody (addCatalysers [X_lty; Y_gtx])

    and compileCommand (env: Environment) : Command -> RxnSystem = function 
        | Composable    c -> c |> compileComposable env
        | NonComposable c -> c |> compileNonComposable env
        | Conditional   c -> c |> compileConditional env