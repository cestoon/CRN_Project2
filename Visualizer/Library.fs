// #############################################
// # Authors: Mads                             #
// # Contributor: Alina & Mads                 #
// # Date: Jun 12th                            #
// # Last edit: June 16th                      #
// #############################################
namespace Visualizer

open Plotly.NET
open State.State
open AST.CRNPP
open Plotly.NET.LayoutObjects
open System.IO

module Visualizer =

    let getSavePath() =
        let folder = "./out"
        let path = $"{folder}/Tree-{System.DateTime.Now.ToFileTime()}.html"
        
        if not (Directory.Exists folder) then Directory.CreateDirectory folder |> ignore
        path

    let getValues (states: seq<int*State>) (species: Species) =
        states
        |> Seq.map (fun (i, state) ->
            match state.TryFind species with
            | Some number -> (i, number)
            | None -> (i, 0.0m) // If species is not present, assign a default y value (e.g., 0.0)
        )

    let plotSpeciesStates
        (states: seq<State>)
        (speciesList: Species list)
        (xAxisMaxRange)
        (lineShape: StyleParam.Shape)
        =
        // Ensure that we only use some states to plot to ensure good performance
        let plotSimLength = 1000
        let states' = 
            states
            |> Seq.cache 
            |> Seq.indexed
            |> Seq.truncate xAxisMaxRange
            |> Seq.filter (fun (i,_) -> i%(xAxisMaxRange/plotSimLength)=0 || i+1=xAxisMaxRange)

        let traces =
            speciesList
            |> List.map (fun species ->
                Chart.Line(xy = getValues states' species, Name = species)
                |> Chart.withLineStyle (Shape = lineShape))

        let xAxisLength = min xAxisMaxRange (Seq.last states' |> fst)
        Chart.combine (traces)
        |> Chart.withXAxis (LinearAxis.init (Range = StyleParam.Range.ofMinMax (0, xAxisLength)))
        |> Chart.saveHtml(getSavePath(), OpenInBrowser = true)


    let plotInterpreterSpeciesStates  states speciesList xAxisMaxRange =
        plotSpeciesStates states speciesList xAxisMaxRange StyleParam.Shape.Hv
    
    let plotSimulatorSpeciesStates  states speciesList xAxisMaxRange =
        plotSpeciesStates states speciesList xAxisMaxRange StyleParam.Shape.Spline
