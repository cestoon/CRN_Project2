namespace AST

(*
module CrnTypes =
    type species = string

    type Expr = species list

    type ConcS = species * float

    type RxnS = Expr * Expr * float

    type CommandS = 
        | LD of species * species
        | ADD of (species * species) * species
        | SUB of (species * species) * species
        | MUL of (species * species) * species
        | DIV of (species * species) * species
        | SQRT of species * species
        | CMP of species * species
        | IFGT of CommandS list
        | IFGE of CommandS list
        | IFEQ of CommandS list
        | IFLT of CommandS list
        | IFLE of CommandS list

    type RootS = Conc of ConcS | Step of CommandS list

    type Crn =  Roots of RootS list
*)
module CRNPP =
    type Species = string
    type Number = float

    type NonComposableS = Cmp of Species * Species

    type ComposableS
        = Ld of Species * Species
        | Add of Species * Species * Species
        | Sub of Species * Species * Species
        | Mul of Species * Species * Species
        | Div of Species * Species * Species
        | Sqrt of Species * Species
    type ConditionalS
        = IfGT of ComposableS list??????????????????????????
        | IfGE of CommandList
        | IfEQ of CommandList
        | IfLT of CommandList
        | IfLE of CommandList
    and Command 
        = Composable of ComposableS
        | NonComposable of NonComposableS
        | Conditional of ConditionalS
    and CommandList = Command list //TODO?: decide if this should e implemented as (Command * Command list) to ensure minimum 1 element

    type Step = Step of CommandList

    type StepList = Step list //TODO?: decide if this should e implemented as (Step * Step list) to ensure minimum 1 element 

    type Conc = Species * Number

    type RootList 
        = ConcS of Conc * RootList
        | StepList of StepList 

    type Crn = Crn of RootList
