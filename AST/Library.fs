// #############################################
// # Authors: Mads                             #
// # Contributor: Mads & Alina                 #
// # Date: Jun 12th                            #
// # Last edit: June 16th                      #
// #############################################
namespace AST

module CRNPP =
    type Species = string
    type Number = decimal

    type NonComposableS = Cmp of Species * Species

    type ComposableS =
        | Ld of Species * Species
        | Add of Species * Species * Species
        | Sub of Species * Species * Species
        | Mul of Species * Species * Species
        | Div of Species * Species * Species
        | Sqrt of Species * Species

    type ConditionalS =
        | IfGT of CommandList
        | IfGE of CommandList
        | IfEQ of CommandList
        | IfLT of CommandList
        | IfLE of CommandList

    and Command =
        | Composable of ComposableS
        | NonComposable of NonComposableS
        | Conditional of ConditionalS

    and CommandList = Command list //TODO?: decide if this should e implemented as (Command * Command list) to ensure minimum 1 element

    type Step = Step of CommandList

    type StepList = Step list //TODO?: decide if this should e implemented as (Step * Step list) to ensure minimum 1 element

    type Conc = Conc of Species * Number

    type RootList = (Conc list * StepList)

    type Crn = Crn of RootList

module Rxn = 
    type Expr = CRNPP.Species list
    type Rxn = Rxn of Expr * Expr * CRNPP.Number

    let inline makeRxn rs ps k = Rxn (rs, ps, k)
    let inline (=>) rs ps = Rxn(rs, ps, 1)
    let inline (=|) rs k = fun ps -> Rxn(rs, ps, k)
    let inline (|=>) f ps = f ps

    let addCatalysers cs (Rxn(rs, ps, k)) = Rxn(cs@rs, cs@ps, k)
    let addCatalyser c (Rxn(rs, ps, k)) = Rxn(c::rs, c::ps, k)

    let private exprToRxnString (expr: Expr) = 
        let strs = 
            expr
            |> List.map string
        if strs.IsEmpty 
        then "Ø"
        else strs |> List.reduce (fun a b -> $"{a} + {b}") 

    let prettyString (Rxn (rs, ps, k)) = 
            let rsString = exprToRxnString rs
            let psString = exprToRxnString ps
            let arrow = if k.Equals(1.) then "--->" else $"-{k}->"
            $"{rsString} {arrow} {psString}"
