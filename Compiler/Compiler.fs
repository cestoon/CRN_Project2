namespace Compiler

open AST.Rxn
open State

/// The different steps (or more gennerally the temporally sepperated reactions)
/// of a system represented by a list ordered by the order in which reactions ocour
type RxnSequence = Rxn list list

/// A selfcontained system of reactions happening concurrently, with each reactant, 
/// and product stored in the environment.
type RxnSystem = Rxn list * Environment
type Compiler<'a> = Environment -> 'a -> RxnSystem

module RxnSystem = 
    let map (f: Rxn -> Rxn) (rxns, env) : RxnSystem = (List.map f rxns, env)
    let addClock ((_, env) as rxnSys) = map (addCatalyser env.stepClock) rxnSys 

    let compileMany compiler env xs : RxnSystem = 
        xs 
        |> List.map (fun c e -> compiler e c)
        |> List.fold 
            (fun (rxns, e) f ->  
                let (rxns', env') = f e
                (rxns'@rxns, env')) 
            ([], env)

    let compileMapMany<'a> (compiler: Compiler<'a>) (f: Rxn -> Rxn) env xs : RxnSystem = 
        xs 
        |> List.map 
            (fun c e -> 
                let (rxns, env') = compiler e c
                let rxns' = List.map (f) rxns
                (rxns', env')) 
        |> List.fold (fun (rxns, e) f -> 
            let (rxns', env') = f e
            (rxns'@rxns, env')) 
            ([], env)
