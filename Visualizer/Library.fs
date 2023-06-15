namespace Visualizer

open Plotly.NET
open State.State
open AST.CRNPP
open Plotly.NET.TraceObjects
open Plotly.NET.LayoutObjects

module Visualizer =
    let plotSpeciesStates (states: seq<State>) (speciesList: Species list) (xAxisMaxRange) =
        let getXValues (states: seq<State>) = states |> Seq.mapi (fun i _ -> i) // Generate x-axis values (state numbers)

        let getYValues (states: seq<State>) (species: Species) =
            states
            |> Seq.map (fun state ->
                match state.TryFind species with
                | Some number -> number
                | None -> 0.0 // If species is not present, assign a default value (e.g., 0.0)
            )

        let traces =
            speciesList
            |> List.map (fun species ->
                Chart.Line(x = getXValues states, y = getYValues states species)
                |> Chart.withLineStyle (Shape = StyleParam.Shape.Hv))

        Chart.combine (traces)
        |> Chart.withXAxis (LinearAxis.init (Range = StyleParam.Range.ofMinMax (0, xAxisMaxRange)))
        |> Chart.show
