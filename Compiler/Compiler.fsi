namespace Compiler

open Compiler.Environment
open AST.CRNPP
open AST.Rxn

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
    val map: (Rxn -> Rxn) -> RxnSystem -> RxnSystem 
    val addClock: RxnSystem -> RxnSystem
module RxnSequence =
    val merge: RxnSequence -> RxnSequence -> RxnSequence

module RxnNetwork =
    val map: (RxnSequence -> RxnSequence) -> RxnNetwork -> RxnNetwork  
    val fromRxnSystem: RxnSystem -> RxnNetwork
    val mapRxns: (Rxn -> Rxn) -> RxnNetwork -> RxnNetwork 
    val compileMany: SequenceCompiler<'a> -> Environment -> 'a list -> RxnNetwork
    val compileMapMany: SequenceCompiler<'a> -> (Rxn -> Rxn) -> Environment -> 'a list -> RxnNetwork