namespace ChemicalSimulator

open AST.CRNPP
open AST.Rxn
open State.State


module ChemicalSimulator =
    let tryGetWithDefault k d m =
        m |> Map.tryFind k |> Option.defaultValue d

    let tryMap k v m =
        m
        |> Map.change k (fun o -> o |> Option.defaultValue 0. |> (fun a -> a + v) |> Some)

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
        |> List.fold
            (fun acc spec -> acc * (state |> tryGetWithDefault spec 0.))
            (float (reactionNetChangeMap |> tryGetWithDefault species 0) * k)

    let calculateDerivative stepSize species (reactions: Rxn list) state =
        reactions
        |> List.map (fun (Rxn(rs, ps, _) as rxn) ->
            let allSpecies = Set.ofList (rs @ ps)
            let reactionNetChangeMap = calculateNetChangePerReaction allSpecies rxn
            (rxn, reactionNetChangeMap))
        |> List.sumBy (fun (rxn, changeMap) -> calculateDerivativePerReaction changeMap species rxn state)
        |> (fun v -> tryMap species (v * stepSize) state)


    let rec simulate stepSize speciesList rxns state =
        let newState =
            speciesList
            |> List.fold (fun state spec -> calculateDerivative stepSize spec rxns state) state

        seq {
            yield newState
            yield! simulate stepSize speciesList rxns newState
        }
