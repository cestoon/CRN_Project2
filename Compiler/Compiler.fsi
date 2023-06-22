// #############################################
// # Authors: Mads                             #
// # Contributor: Mads;                        #
// # Date: Jun 16th                            #
// # Last edit: June 22th                      #
// #############################################
namespace Compiler

open State
open AST.Rxn

/// A selfcontained system of reactions happening concurrently, with each reactant, 
/// and product stored in the environment.
type RxnSystem = Rxn list * Environment
type Compiler<'a> = Environment -> 'a -> RxnSystem



module RxnSystem =
    val map: (Rxn -> Rxn) -> RxnSystem -> RxnSystem 
    val addClock: RxnSystem -> RxnSystem
    val compileMany: Compiler<'a> -> Environment -> 'a list -> RxnSystem
    val compileMapMany: Compiler<'a> -> (Rxn -> Rxn) -> Environment -> 'a list -> RxnSystem