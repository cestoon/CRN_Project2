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

let basicArbitraryTest (species1:Species) (species2:Species) (species3:Species) (species4:Species) (value1:Number) (value2:Number) (value3:Number) (value4:Number) =
    let aCrn =
        Crn([Conc (species1, value1); Conc (species2, value2); Conc (species3, value3); Conc (species4, value4)],
        [Step
            [Composable (Add (species1, species2, "resAdd"));
            Composable (Sub (species3, species4, "resSub"))];
        Step
            [NonComposable (Cmp ("resAdd", "resSub"))];
        Step
            [Conditional (IfGT
                [Composable (Mul ("resAdd", species1, species2))]);
            Conditional (IfLE 
                [Composable (Div ("resSub", species3, species4));
                Composable (Sqrt ("resAdd", species1))])]])

    let aState = State([])
    let res = executeCRN aState aCrn
    printfn "%A" res |> ignore
    let finalState = List.last (List.ofSeq res)
    printfn "%A" finalState.[species1] |> ignore
    printfn "%A" finalState.[species2] |> ignore
    printfn "%A" finalState.[species3] |> ignore
    printfn "%A" finalState.[species4] |> ignore
    printfn "%A" finalState.["resAdd"] |> ignore
    printfn "%A" finalState.["resSub"] |> ignore

basicArbitraryTest "a1" "2b" "cc33" "44DDDD" 0.1 0.2 2.3 4.5