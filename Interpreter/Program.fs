namespace Interpreter

open AST.CRNPP
open State.State

module Execute =
    let executeComposableCommand (state: State) (command: ComposableS) =
        match command with
        | Ld(species, _) -> state // TODO: Implement loading logic
        | Add(species1, species2, species3) -> state // TODO: Implement addition logic
        | Sub(species1, species2, species3) -> state // TODO: Implement subtraction logic
        | Mul(species1, species2, species3) -> state // TODO: Implement multiplication logic
        | Div(species1, species2, species3) -> state // TODO: Implement division logic
        | Sqrt(species1, species2, species3) -> state // TODO: Implement square root logic

    let executeNonComposableCommand (state: State) (command: NonComposableS) =
        match command with
        | Cmp(species1, species2) -> state // TODO: Implement non-composable command logic

    let executeConditionalCommand (state: State) (command: ConditionalS) =
        match command with
        | IfGT commandList -> state // TODO: Implement "greater than" conditional logic
        | IfGE commandList -> state // TODO: Implement "greater than or equal to" conditional logic
        | IfEQ commandList -> state // TODO: Implement "equal to" conditional logic
        | IfLT commandList -> state // TODO: Implement "less than" conditional logic
        | IfLE commandList -> state // TODO: Implement "less than or equal to" conditional logic

    let executeCommand (state: State) (command: Command) =
        match command with
        | Composable composableCommand -> executeComposableCommand state composableCommand
        | NonComposable nonComposableCommand -> executeNonComposableCommand state nonComposableCommand
        | Conditional conditionalCommand -> executeConditionalCommand state conditionalCommand

    let executeStep (state: State) (step: Step) =
        let rec executeCommands state' commands =
            match commands with
            | [] -> state'
            | command :: remainingCommands ->
                let nextState = executeCommand state' command
                executeCommands nextState remainingCommands

        match step with
        | Step commandList -> executeCommands state commandList

    let executeStepList (state: State) (stepList: StepList) =
        let rec executeSteps state' steps =
            match steps with
            | [] -> state'
            | step :: remainingSteps ->
                let nextState = executeStep state' step
                executeSteps nextState remainingSteps

        executeSteps state stepList

    let executeConc (state: State) (conc: Conc) = conc :: state

    let rec executeRootList (state: State) (rootList: RootList) =
        match rootList with
        | ConcS(conc, next) ->
            let newState = executeConc state conc
            executeRootList newState next
        | StepList stepList ->
            let nextState = executeStepList state stepList
            executeRootList nextState rootList

    let rec executeCRN (state: State) (crn: Crn) =
        match crn with
        | Crn rootList -> executeRootList state rootList



// // Example usage:
// let initialState = []

// let crn =
//     Crn(ConcS(("Species1", 10.0), StepList [ Step [ Composable(Ld("Species1", "Species2")) ] ]))

// let result = executeCRN initialState crn
// printfn "Result: %A" result
