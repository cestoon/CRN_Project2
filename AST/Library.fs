namespace AST

module CrnTypes =
    let hello name =
        printfn "Hello %s" name


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
