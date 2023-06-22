// #############################################
// # Authors: Mads                             #
// # Contributor: Alina;                       #
// # Date: Jun 12th                            #
// # Last edit: June 16th                      #
// #############################################
namespace Visualizer

open Plotly.NET
open State.State
open AST.CRNPP
open Plotly.NET.TraceObjects
open Plotly.NET.LayoutObjects

module Visualizer =

    let getLineShape line =
        match line with
        | "Hv" -> StyleParam.Shape.Hv
        | "Hvh" -> StyleParam.Shape.Hvh
        | "Vh" -> StyleParam.Shape.Vh
        | "Vhv" -> StyleParam.Shape.Vhv
        | "Spline" -> StyleParam.Shape.Spline
        | "Linear" -> StyleParam.Shape.Linear
        | _ -> failwith "No such style param"


    let plotSpeciesStates
        (states: seq<State>)
        (speciesList: Species list)
        (xAxisMaxRange)
        (lineShape: StyleParam.Shape)
        =
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
                Chart.Line(x = getXValues states, y = getYValues states species, Name = species)
                |> Chart.withLineStyle (Shape = lineShape))

        Chart.combine (traces)
        |> Chart.withXAxis (LinearAxis.init (Range = StyleParam.Range.ofMinMax (0, xAxisMaxRange)))
        |> Chart.show
