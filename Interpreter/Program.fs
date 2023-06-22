// #############################################
// # Authors: Mads                             #
// # Contributor: Alina;                       #
// # Date: Jun 12th                            #
// # Last edit: June 16th                      #
// #############################################
namespace Interpreter

open AST.CRNPP
open State.State

module Execute =
    let executeComposableCommand (state: State) (command: ComposableS) =
        match command with
        | Ld(species1, species2) -> state.Add(species2, state[species1])
        | Add(species1, species2, species3) ->
            let newSpeciesValue = state[species1] + state[species2]
            state.Add(species3, newSpeciesValue)

        | Sub(species1, species2, species3) ->
            let newSpeciesValue = state[species1] - state[species2]
            state.Add(species3, newSpeciesValue)

        | Mul(species1, species2, species3) ->
            let newSpeciesValue = state[species1] * state[species2]
            state.Add(species3, newSpeciesValue)

        | Div(species1, species2, species3) ->
            let newSpeciesValue = state[species1] / state[species2]
            state.Add(species3, newSpeciesValue)

        | Sqrt(species1, species2) ->
            let newSpeciesValue = sqrt state[species1]
            state.Add(species2, newSpeciesValue)

    let executeNonComposableCommand (state: State) (command: NonComposableS) =
        match command with
        | Cmp(species1, species2) ->
            let total = state[species1] + state[species2]
            let xMapping = state[species1] / total
            let yMapping = state[species2] / total

            state |> Map.add "XgtY" xMapping |> Map.add "XltY" yMapping

    let (.=) a b = a.Equals(b)
    let (.<=) a b = a < b || a .= b
    let (.>=) a b = a > b || a .= b

    let rec executeConditionalCommand (state: State) (command: ConditionalS) =
        let XgtYValue = state["XgtY"]
        let XltYValue = state["XltY"]

        let conditionalExec b cs =
            if b XgtYValue XltYValue then
                executeCommands state cs
            else
                state

        match command with
        | IfGT commandList -> conditionalExec (>) commandList
        | IfGE commandList -> conditionalExec (.>=) commandList
        | IfEQ commandList -> conditionalExec (.=) commandList
        | IfLT commandList -> conditionalExec (<) commandList
        | IfLE commandList -> conditionalExec (.<=) commandList

    and executeCommand (state: State) (command: Command) =
        match command with
        | Composable composableCommand -> executeComposableCommand state composableCommand
        | NonComposable nonComposableCommand -> executeNonComposableCommand state nonComposableCommand
        | Conditional conditionalCommand ->

            executeConditionalCommand state conditionalCommand

    and executeCommands state' commands =
        match commands with
        | [] -> state'
        | command :: remainingCommands ->
            let nextState = executeCommand state' command
            executeCommands nextState remainingCommands

    let executeStep (state: State) (step: Step) =
        match step with
        | Step commandList -> executeCommands state commandList

    let executeStepList (state: State) (stepList: StepList) =
        let rec executeSteps (state': State) steps =
            match steps with
            | [] -> Seq.empty
            | step :: remainingSteps ->
                let nextState = executeStep state' step

                seq {
                    yield nextState
                    yield! executeSteps nextState remainingSteps
                }

        executeSteps state stepList

    let executeConc (state: State) (Conc(species, amount)) : State = state.Add(species, amount)

    let rec executeMainLoop state stepList =
        seq {
            let states = executeStepList state stepList
            yield! states
            yield! executeMainLoop (Seq.last states) stepList
        }

    let executeRootList (state: State) ((concList, stepList): RootList) : seq<State> =
            let initialConcState = List.fold executeConc state concList
            executeMainLoop initialConcState stepList

    let rec executeCRN (state: State) (crn: Crn) =
        match crn with
        | Crn rootList -> executeRootList state rootList
