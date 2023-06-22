// #############################################
// # Authors: Alina & Mads                     #
// # Date: Jun 12th                            #
// # Last edit: June 16th                      #
// #############################################
namespace ChemicalSimulator

open AST.CRNPP
open AST.Rxn
open State.State
open State.Environment
open State


module ChemicalSimulator =
    let tryGetWithDefault k d m =
        m |> Map.tryFind k |> Option.defaultValue d

    let tryMap k v m =
        m
        |> Map.change k (fun o -> 
            o 
            |> Option.orElse (Some 0m) 
            |> Option.map (fun a -> a + v))
    
    let netChange s m = m |> tryGetWithDefault s 0

    let ( ** ) d = function
        | 0 -> 1m
        | 1 -> d
        | n -> 
            d
            |> List.replicate n
            |> List.reduce (fun a b -> a * b) 

    let calculateNetChangePerReaction allSpecies (Rxn(rs, ps, k)) =
        let rsMultiplicity = rs |> List.countBy (id) |> Map.ofList

        let psMultiplicity = ps |> List.countBy (id) |> Map.ofList

        allSpecies
        |> Set.fold
            (fun rcs spec ->
                rcs
                |> Map.add
                    spec
                    ((psMultiplicity |> tryGetWithDefault spec 0)
                     - (rsMultiplicity |> tryGetWithDefault spec 0)))
            Map.empty


    let calculateDerivativePerReaction
        (reactionNetChangeMap: Map<Species, int>)
        (species: Species)
        (Rxn(rs, _, k))
        (state: State)
        =
        rs
        |> List.countBy id
        |> List.fold
            (fun acc (spec, m_rxn) -> acc * ((state |> tryGetWithDefault spec 0m) ** m_rxn))
            (decimal (netChange species reactionNetChangeMap) * k)

    let calculateDerivative stepSize species (reactions: Rxn list) state =
        reactions
        |> List.map (fun (Rxn(rs, ps, _) as rxn) ->
            let allSpecies = Set.ofList (rs @ ps)
            let reactionNetChangeMap = calculateNetChangePerReaction allSpecies rxn
            (rxn, reactionNetChangeMap))
        |> List.sumBy (fun (rxn, changeMap) -> calculateDerivativePerReaction changeMap species rxn state)

    let simulate stepSize (rxns, env)  =
        let allSpecies = getAllSpecies env
        let calculateSpeciesDerivative state spec  = calculateDerivative stepSize spec rxns state

        let rec simulate' state =
            let newState =
                allSpecies
                |> Set.map (fun spec -> 
                    let derivative = calculateSpeciesDerivative state spec
                    let speciesChange = derivative * stepSize 
                    (spec, speciesChange))  
                |> Set.fold (fun state (spec, change) -> state |> tryMap spec change) state
            seq {
                yield state
                yield! simulate' newState
            }
        
        simulate' env.initialConcentrations
    
    let simulateWithInitialState stepSize (rxns, env) state =
        let addValue m k v = m |> Map.add k v

        let env' =
            { env with initialConcentrations = state |> Map.fold (addValue) env.initialConcentrations }
        simulate stepSize (rxns, env')
