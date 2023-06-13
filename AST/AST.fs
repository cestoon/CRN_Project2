type Expr = 
    | Species of string
    | Add of Species * Species

type ConditionalS = 
    | IfGT of CommandSList
    | IfGE of CommandSList
    | IfEQ of CommandSList
    | IfLT of CommandSList
    | IfLE of CommandSList

and CommandS = 
    | RxnS of Expr * Expr * float  // reaction speed
    | ArithmeticS of ModuleS
    | CmpS of ModuleS
    | ConditionalS of ConditionalS

and ModuleS = 
    | LD of Species * Species
    | Add of Species * Species * Species
    | Sub of Species * Species * Species
    | Mul of Species * Species * Species
    | Div of Species * Species * Species
    | Sqrt of Species * Species
    | Cmp of Species * Species

and CommandSList = CommandS list

type StepS = StepS of CommandSList
type ConcS = ConcS of Species * float  // concentrations
type RootS = ConcS | StepS

type RootSList = RootS list
type Crn = Crn of RootSList
