namespace Compiler

open AST

module Environment =
    type ComparisonSpecies = { 
        Xgty: CRNPP.Species; 
        Xlty: CRNPP.Species; 
        Ygtx: CRNPP.Species;
        Yltx: CRNPP.Species;
        ComparisonOffset: CRNPP.Species;
        B: CRNPP.Species }
    
    type Environment = 
        { species: Set<CRNPP.Species>

          helperSpecies: Set<CRNPP.Species>
          helperSpeciesGen: seq<CRNPP.Species>
  
          clockSpecies: List<CRNPP.Species> 
          clockCount: int  
          stepClock: CRNPP.Species
          
          flagSpecies: ComparisonSpecies }

    let rec getNewHelperSpecies (env: Environment) = 
        let newSpec = Seq.head env.helperSpeciesGen
        let specExists = env.helperSpecies.Contains newSpec
        let env' = { 
            env with
                helperSpecies = Set.add newSpec env.helperSpecies
                helperSpeciesGen = Seq.skip 1 env.helperSpeciesGen
        } 
        if specExists
        then getNewHelperSpecies env'
        else (newSpec, env')

    let private makeClockN n = $"clock_{n}"  

    let getNewClock (env: Environment) = 
        let offset = env.clockCount
        let clock1 = makeClockN (1 + offset)
        let clock2 = makeClockN (2 + offset)
        let clock3 = makeClockN (3 + offset)

        let clocks' = clock3::clock2::clock1::env.clockSpecies
        let env' = {env with clockSpecies = clocks'; clockCount = env.clockCount + 3}
        (clock3, env')

    let getNewStepClock env = 
        let (clock, env') = getNewClock env
        (clock, { env' with stepClock = clock})

    let newUninitializedEnv = 
        {   species = Set.empty

            helperSpecies = Set.empty
            helperSpeciesGen = Seq.initInfinite (fun i -> $"GenSpec_{i}")

            clockSpecies = []
            clockCount = 0
            stepClock = "Uninitialized"

            flagSpecies = 
                { Xgty = "X_gtY";  
                    Xlty = "X_ltY"; 
                    Ygtx = "Y_gtX"; 
                    Yltx = "Y_ltX"; 
                    ComparisonOffset = "ComparisonOffset"; 
                    B = "ComparisonResult" } }

    let newEnv = 
        newUninitializedEnv 
        |> getNewStepClock
        |> snd