// #############################################
// # Authors: Mads                             #
// # Contributor: Mads                         #
// # Date: Jun 16th                            #
// # Last edit: June 16th                      #
// #############################################
namespace Compiler

open AST.CRNPP
open Compiler

module CrnCommands =
    val compileComposable: Compiler<ComposableS>
    val compileNonComposable: Compiler<NonComposableS>
    val compileConditional: Compiler<ConditionalS>

    val compileCommand: Compiler<Command>