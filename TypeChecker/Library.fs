namespace TypeChecker
open AST.CRNPP

module TypeChecker = 
//There must be no cyclical dependencies within a step
//A species cannot be the result of more than one module per step
//Conditional must not be in the same step with a comparison
//Conditional must be only in steps after comparison
    let checkComposable (composable: ComposableS): (Species list * Species) =
        match composable with
        |Ld (species1, species2) -> [species1], species2
        |Add (species1, species2, species3) -> [species1; species2], species3
        | Sub (species1, species2, species3) -> [species1; species2], species3
        | Mul (species1, species2, species3) -> [species1; species2], species3
        | Div (species1, species2, species3) -> [species1; species2], species3
        | Sqrt (species1, species2) -> [species1], species2

    let rec checkCommandList (commands: CommandList) (declaredSpecies: Species list) (isError:bool) (outputs: Species list) (hasCmp: bool) (hasConditional: bool) =
        //There must be no cyclical dependencies within a step
        //A species cannot be the result of more than one module per step
        match commands with
        |[] -> declaredSpecies, isError, outputs, hasCmp, hasConditional
        |command::tail ->
            match command with
            |Composable composable ->
                let inputs, output = checkComposable composable
                //declaredSpecies should include inputs, otherwise there is cyclical dependency
                let isCyclicalDependency = (Set.ofList inputs) <> (Set.intersect (Set.ofList inputs) (Set.ofList declaredSpecies))
                if isCyclicalDependency then printfn "cyclical dependency"
                let isConflictOutput = List.contains output outputs
                if isConflictOutput then printfn "found result species conflict"
                checkCommandList tail (output::declaredSpecies) (isError || isConflictOutput || isConflictOutput) (output::outputs) hasCmp hasConditional

            |NonComposable (Cmp(species1, species2))->
                let inputs = [species1; species2]
                //declaredSpecies should include inputs, otherwise there is cyclical dependency
                let isCyclicalDependency = (Set.ofList inputs) <> (Set.intersect (Set.ofList inputs) (Set.ofList declaredSpecies))
                if isCyclicalDependency then printfn "found cyclical dependency"
                checkCommandList tail declaredSpecies (isError || isCyclicalDependency) outputs true hasConditional

            |Conditional conditional ->
                match conditional with
                |IfGT commands ->
                    let declaredSpecies,isError, outputs, hasCmp, _ =
                        checkCommandList commands declaredSpecies isError outputs hasCmp hasConditional
                    checkCommandList tail declaredSpecies isError outputs hasCmp true
                |IfGE commands ->
                    let declaredSpecies, isError, outputs, hasCmp, _ =
                        checkCommandList commands declaredSpecies isError outputs hasCmp hasConditional
                    checkCommandList tail declaredSpecies isError outputs hasCmp true
                |IfEQ commands ->
                    let declaredSpecies, isError, outputs, hasCmp, _ =
                        checkCommandList commands declaredSpecies isError outputs hasCmp hasConditional
                    checkCommandList tail declaredSpecies isError outputs hasCmp true
                |IfLT commands ->
                    let declaredSpecies, isError, outputs, hasCmp, _ =
                        checkCommandList commands declaredSpecies isError outputs hasCmp hasConditional
                    checkCommandList tail declaredSpecies isError outputs hasCmp true
                |IfLE commands ->
                    let declaredSpecies, isError, outputs, hasCmp, _ =
                        checkCommandList commands declaredSpecies isError outputs hasCmp hasConditional
                    checkCommandList tail declaredSpecies isError outputs hasCmp true

    let rec checkSteps (steps: StepList) (declaredSpecies: Species list) (isError:bool) (prevHasCmp: bool): bool =
    //Conditional must not be in the same step with a comparison
    //Conditional must be only in steps after comparison
        match steps with
        |[] -> isError
        |step::tail ->
            match step with
            |Step([]) -> checkSteps tail declaredSpecies isError false
            |Step(commands) ->
                let declaredSpecies, isError, _, hasCmp, hasConditional = 
                    checkCommandList commands declaredSpecies isError [] false false
                match prevHasCmp, hasCmp, hasConditional with
                |_, true, true ->
                    printfn "found Conditional and Cmp in same step"
                    checkSteps tail declaredSpecies true hasCmp
                |false, _, true ->
                    printfn "Cmp is required before Conditional"
                    checkSteps tail declaredSpecies true hasCmp
                |_, _, _ ->
                    checkSteps tail declaredSpecies isError hasCmp

    let checkConc (Conc(species, _)) =
        species

    let checkCrn (crn: Crn): bool =
        match crn with
        |Crn(concs, steps) ->
            let declaredSpecies = List.map checkConc concs
            checkSteps steps declaredSpecies false false
