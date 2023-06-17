#load "../AST/Library.fs";;
#load "../State/Library.fs";;
#load "Program.fs";;

open System;;
open AST.CRNPP;;
open State.State;;
open Interpreter.Execute;;

0

let randomSwap (step: Step) =
    let random = Random()
    match step with
    |Step(commands) ->
        match commands with
        | [] | [_] -> Step(commands) // No or only one element, no swap needed
        | _ ->
            let index1 = random.Next(0, (List.length commands))
            let index2 = 
                let rec findIndex (index: int) =
                    let newIndex = random.Next(0, (List.length commands))
                    if newIndex = index then findIndex index
                    else newIndex
                findIndex index1
            
            let resCommands = 
                commands
                |> List.mapi (fun i x ->
                    if i = index1 then List.item index2 commands
                    elif i = index2 then List.item index1 commands
                    else x
                )
            Step(resCommands)

let checkRandomOrderCommands () =

    let rec iterateSteps (steps: StepList) =
        match steps with
        |[] -> []
        |step::tail ->
            let swappedStep = randomSwap step
            swappedStep :: (iterateSteps tail)

    let approxPiStr = 
        Crn([Conc ("four", 4.0); Conc ("divisor1", 1.0); Conc ("divisor2", 3.0);
            Conc ("pi", 0.0)],
        [Step
            [Composable (Div ("four", "divisor1", "factor1"));
            Composable (Add ("divisor1", "four", "divisor1Next"));
            Composable (Div ("four", "divisor2", "factor2"));
            Composable (Add ("divisor2", "four", "divisor2Next"));
            Composable (Sub ("factor1", "factor2", "factor"));
            Composable (Add ("pi", "factor", "piNext"))];
        Step
            [Composable (Ld ("divisor1Next", "divisor1"));
            Composable (Ld ("divisor2Next", "divisor2"));
            Composable (Ld ("piNext", "pi"))]])

    let aState = State([])
    let resOrigin = executeCRN aState approxPiStr
    printfn "%A" approxPiStr |> ignore
    printfn "%A" resOrigin |> ignore

    match approxPiStr with
    |Crn(concs, steps) ->
        let swappedCrn = Crn(concs, (iterateSteps steps))

        let aState = State([])
        let resSwapped = executeCRN aState swappedCrn
        printfn "%A" swappedCrn |> ignore
        printfn "%A" resSwapped |> ignore
        printfn "%A" (resSwapped = resOrigin) |> ignore
        //Sequence cannot be compared with =
        //so compare the printed string
        let resOriginStr = sprintf "%A" resOrigin
        let resSwappedStr = sprintf "%A" resSwapped
        printfn "%A" (resSwappedStr = resOriginStr) |> ignore

checkRandomOrderCommands ()
