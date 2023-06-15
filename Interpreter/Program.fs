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

    // let executeNonComposableCommand (state: State) (command: NonComposableS) =
    //     match command with
    //     | Cmp(species1, species2) ->
    //         let ge, lt =
    //             if state[species1] >= state[species2] then
    //                 state[species1], state[species2]
    //             else
    //                 state[species2], state[species1]

    //         state |> Map.add "XgtY" ge |> Map.add "XltY" lt

    let executeNonComposableCommand (state: State) (command: NonComposableS) =
        match command with
        | Cmp(species1, species2) ->
            let total = state[species1] + state[species2]
            let xMapping = state[species1] / total
            let yMapping = state[species2] / total

            state |> Map.add "XgtY" xMapping |> Map.add "XltY" yMapping

    let rec executeConditionalCommand (state: State) (command: ConditionalS) =
        let XgtYValue = state["XgtY"]
        let XltYValue = state["XltY"]

        printfn "valueOf XgtYValue: %A" XgtYValue
        printfn "valueOf XltYValue: %A" XltYValue

        let conditionalExec b cs =
            if b XgtYValue XltYValue then
                executeCommands state cs
            else
                state

        match command with
        | IfGT commandList -> conditionalExec (>) commandList
        | IfGE commandList -> conditionalExec (>=) commandList
        | IfEQ commandList -> conditionalExec (=) commandList
        | IfLT commandList -> conditionalExec (<) commandList
        | IfLE commandList -> conditionalExec (<=) commandList

    and executeCommand (state: State) (command: Command) =
        match command with
        | Composable composableCommand ->
            printfn "Execute Composable"
            executeComposableCommand state composableCommand
        | NonComposable nonComposableCommand ->
            printfn "Execute NonComposable"
            executeNonComposableCommand state nonComposableCommand
        | Conditional conditionalCommand ->
            printfn "Execute conditional"
            executeConditionalCommand state conditionalCommand

    and executeCommands state' commands =
        printfn "Execute Commands"

        match commands with
        | [] -> state'
        | command :: remainingCommands ->
            let nextState = executeCommand state' command
            executeCommands nextState remainingCommands

    let executeStep (state: State) (step: Step) =
        match step with
        | Step commandList ->
            printfn "Execute Step"
            executeCommands state commandList

    let executeStepList (state: State) (stepList: StepList) =
        let rec executeSteps (state': State) steps =
            printfn "Execute StepList %A " stepList.Length
            printfn "StepList %A " stepList

            match steps with
            | [] -> Seq.empty
            | step :: remainingSteps ->
                let nextState = executeStep state' step

                seq {
                    yield nextState
                    yield! executeSteps nextState remainingSteps
                }

        executeSteps state stepList

    let executeConc (state: State) (conc: Conc) : State = state.Add(conc)


    // let executeConcList (state: State) (concList: Conc list) =
    //     let rec executeConcs (state': State) concs =
    //         match concs with
    //         | [] -> Seq.empty
    //         | conc :: remainingConcs ->
    //             let nextState = executeConc state' conc

    //             seq {
    //                 yield nextState
    //                 yield! executeConcs nextState remainingConcs
    //             }

    //     executeConcs state concList


    // let executeConcs (state: State) (concList: Conc list) =
    //     let rec executeConcs state' concs =
    //         match concs with
    //         | [] -> state'
    //         | conc :: remainingConcs ->
    //             let nextState = executeConc state' conc
    //             executeConcs nextState remainingConcs

    //     executeConcs state concList
    let rec executeMainLoop state stepList =
        seq {
            let states = executeStepList state stepList
            yield! states
            yield! executeMainLoop (Seq.last states) stepList
        }

    let rec executeRootList (state: State) (rootList: RootListS) : seq<State> =
        match rootList with
        | RootList(concList, stepList) ->
            let initialConcState = List.fold executeConc state concList
            executeMainLoop initialConcState stepList
    // let nextState = executeStepList initialConcState stepList

    // seq {
    //     for state in nextState do
    //         yield state

    //     yield! executeStepList (Seq.last nextState) stepList
    // }


    let rec executeCRN (state: State) (crn: Crn) =
        match crn with
        | Crn rootList -> executeRootList state rootList
