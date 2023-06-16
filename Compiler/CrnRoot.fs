namespace Compiler

open AST.CRNPP
open AST.Rxn

open Compiler.Environment

module CrnRoot =
    let addConc env (spec, _) = { env with species = Set.add spec env.species }
    let addConcList env concs = concs |> List.fold (addConc) env

    let compileStep env (Step(cs)) = 
        let (_, env') = getNewStepClock env
        cs 
        |> RxnNetwork.compileMany (CrnCommands.compileCommand) env'

    let compileStepList env steps = 
        steps 
        |> RxnNetwork.compileMany (compileStep) env 

    let compileRootList env (RootList(concs, steps)) = 
        env
        |> (fun e -> addConcList e concs)
        |> (fun e -> compileStepList e steps)

    let compile (Crn(rootlist)) = 
        compileRootList newUninitializedEnv rootlist