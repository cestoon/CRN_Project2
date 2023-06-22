// #############################################
// # Authors: Alina                            #
// # Date: Jun 12th                            #
// # Last edit: June 14th                      #
// #############################################
namespace State

open AST.CRNPP

module State =
    type State = Map<Species, Number>
