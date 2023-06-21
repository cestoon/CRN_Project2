module InterpreterTests

open NUnit.Framework
open System
open AST.CRNPP
open State.State
open Interpreter.Execute
open TypeChecker.TypeChecker

[<SetUp>]
let Setup () =
    ()

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

[<TestCase>]
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
        resSwappedStr = resOriginStr

let calc (value1:float) value2 value3 value4 = 
    let resAdd = value1 + value2
    match value3, value4 with
    |value3, value4 when value3 - value4 < 0.0 ->
        let resSub = 0.0
        match resAdd, resSub with
        |resAdd, resSub when resAdd > resSub  ->
            let value2 = resAdd * value1
            value1, value2, resSub, value4, resAdd, resSub
        |_,_ ->
            let value4 = resSub / value3
            let value1 = sqrt resAdd
            value1, value2, resSub, value4, resAdd, resSub
    |_,_ -> 
        let resSub = value3 - value4
        match resAdd, resSub with
        |resAdd, resSub when resAdd > resSub  ->
            let value2 = resAdd * value1
            value1, value2, resSub, value4, resAdd, resSub
        |_,_ ->
            let value4 = resSub / value3
            let value1 = sqrt resAdd
            value1, value2, resSub, value4, resAdd, resSub

[<Property>]
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
                Composable (Sqrt ("resAdd", species1))])];
        Step
            [Composable (Ld ("resSub", species3))] ])

    let aState = State([])
    let res = executeCRN aState aCrn
    printfn "%A" res |> ignore
    printfn "after res" |> ignore
    //let resCheck = checkCrn aCrn
    //printfn "%A" resCheck |> ignore
    match aCrn with
    |Crn(concs, steps) ->
        let res = Seq.truncate (steps.Length) res
        let finalState = List.last (List.ofSeq res)
        printfn "before List.ofSeq res" |> ignore
        printfn "%A" (List.ofSeq res) |> ignore
        let value1, value2, value3, value4, resAdd, resSub = calc value1 value2 value3 value4 
        printfn "%A" (finalState.[species1] = value1) |> ignore
        printfn "%A" (finalState.[species2] = value2) |> ignore
        printfn "%A" (finalState.[species3] = value3) |> ignore
        printfn "%A" (finalState.[species4] = value4) |> ignore
        printfn "%A" (finalState.["resAdd"] = resAdd) |> ignore
        printfn "%A" (finalState.["resSub"] = resSub) |> ignore
        finalState.[species1] = value1 && finalState.[species2] = value2 &&
        finalState.[species3] = value3 && finalState.[species4] = value4 &&
        finalState.["resAdd"] = resAdd && finalState.["resSub"] = resSub