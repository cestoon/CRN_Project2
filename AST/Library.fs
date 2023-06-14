namespace AST

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
        = IfGT of CommandList
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

    type RootListS = RootList of (Conc list * Step list)

    type Crn = Crn of RootListS
