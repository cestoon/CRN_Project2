// #############################################
// # Authors: Mads                             #
// # Contributor: Mads                         #
// # Date: Jun 16th                            #
// # Last edit: June 16th                      #
// #############################################
namespace Compiler
open Compiler.Environment
open AST.CRNPP
open AST.Rxn
open Compiler

module CrnCommands =
    val compileComposable: ReactionsCompiler<ComposableS>
    val compileNonComposable: SequenceCompiler<NonComposableS>
    val compileConditional: SequenceCompiler<ConditionalS>

    val compileCommand: SequenceCompiler<Command>