// #############################################
// # Authors: Mads                             #
// # Date: Jun 16th                            #
// # Last edit: June 22th                      #
// #############################################
namespace Compiler

open AST.CRNPP
open State
open State.Environment

module CrnRoot =   
    let addConc env (Conc(spec, conc)) = 
        { env with 
            species = Set.add spec env.species 
            initialConcentrations = env.initialConcentrations |> Map.add spec conc
        }
    let addConcList env concs = concs |> List.fold (addConc) env

    let compileStep env (Step(cs)) = 
        let (_, env') = getNewStepClock env
        cs 
        |> RxnSystem.compileMany (CrnCommands.compileCommand) env'

    let compileStepList env steps = 
        steps 
        |> RxnSystem.compileMany (compileStep) env 

    let compileRootList env (concs, steps) = 
        env
        |> (fun e -> addConcList e concs)
        |> (fun e -> compileStepList e steps)
