namespace Compiler

open AST.CRNPP
open AST.Rxn
open Environment
open Compiler

/// A list of reactions happening concurrently
type ConcurrentRxns = Rxn list

/// The different steps (or more gennerally the temporally sepperated reactions)
/// of a system represented by a list ordered by the order in which reactions ocour
type RxnSequence = ConcurrentRxns list

/// A selfcontained system of reactions happening concurrently, with each reactant, 
/// and product stored in the environment.
type RxnSystem = ConcurrentRxns * Environment
type ReactionsCompiler<'a> = Environment -> 'a -> RxnSystem

/// A network of reactions happening in sequence.
/// The environment stores information about all the species in the network and
/// which clocks, flags and helper species are in use.
type RxnNetwork = RxnSequence * Environment
type SequenceCompiler<'a> = Environment -> 'a -> RxnNetwork

module RxnSystem = 
    let map (f: Rxn -> Rxn) (rxns, env) : RxnSystem = (List.map f rxns, env)
    let addClock ((_, env) as rxnSys) = map (addCatalyser env.stepClock) rxnSys 

module RxnSequence =
    let rec merge s1 s2 : RxnSequence =
        match s1, s2 with
        | [], [] -> []
        | xs, [] | [], xs -> xs
        | x::xs, y::ys -> (x@y)::merge xs ys

module RxnNetwork = 
    let map (f: RxnSequence -> RxnSequence) (rxnSeq, env) : RxnNetwork = (f rxnSeq, env)
    let mapRxns f (network: RxnNetwork) = map (List.map (List.map f)) network
    let fromRxnSystem (rxns, env) : RxnNetwork = ([rxns], env)
    let compileMany compiler env xs : RxnNetwork = 
        xs 
        |> List.map (fun c e -> compiler e c)
        |> List.fold (fun (ccRxns, env) f ->  f env |> map (RxnSequence.merge ccRxns)) ([], env)

    let compileMapMany compiler f env xs : RxnNetwork = 
        xs 
        |> List.map (fun c e -> compiler e c |> mapRxns f)
        |> List.fold (fun (ccRxns, env) f ->  f env |> map (RxnSequence.merge ccRxns)) ([], env)