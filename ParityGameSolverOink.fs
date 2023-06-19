module ParityGameLib.ParityGameSolverOink

open System 
open System.IO

open Util 
open ParityGame

exception ParityGameSolvingException of String

let private parseSolution (s : String) = 
    s.Split('\n')
    |> Array.toList
    |> List.filter (fun x -> x <> "")
    // Drop the first line, as this only contains the number of states
    |> List.tail
    |> List.map (fun x -> 
        // Remove the semicolon at the end and split at each space
        let temp = x[0..x.Length - 2].Split ' '

        let id = 
            try 
                Int32.Parse(temp.[0])
            with 
            | _ -> 
                raise <| ParityGameSolvingException $"Could not parse %s{temp.[0]} into a state (number)"
        
        let winner = 
            match temp.[1] with 
            | "0" -> PlayerZero
            | "1" -> PlayerOne
            | _ -> raise <| ParityGameSolvingException $"Could not parse %s{temp.[1]} to a player (either 0 or 1)"
            
        let suc = 
            if temp.Length <= 2 then 
                None 
            else 
                try 
                    Int32.Parse(temp.[2])
                    |> Some
                with 
                | _ -> 
                    raise <| ParityGameSolvingException $"Could not parse %s{temp.[2]} into a successor state (number)"

        (id, (winner, suc))
        )
    |> Map.ofList

let solveAndComputeStrategyWithOink intermediateFilesPath oinkPath (pg : ParityGame<'T>) : ParityGameSolutionWithStrategy<'T> = 

    let a, b = 
        pg.Properties.Keys
        |> Seq.toList
        |> List.mapi (fun i x -> 
            (x, i), (i, x))
        |> List.unzip

    let d = Map.ofSeq a 
    let revd = Map.ofSeq b

    let sw = new StringWriter()

    sw.WriteLine ("parity " + string(d.Count) + ";")

    for s in pg.Properties.Keys do 
        let sucs, player, color = pg.Properties.[s]
        let id = d.[s]

        let sucString = 
            sucs 
            |> Seq.toList
            |> List.map (fun x -> string(d.[x]))
            |> Util.combineStringsWithSeperator ","

        sw.WriteLine(string(id) + " " + string(color) + " " + ParityGamePlayer.asString player + " " + sucString + " " + "\"\"" + ";")


    let path = Path.Combine [|intermediateFilesPath; "game.txt"|]
    let solPath = Path.Combine [|intermediateFilesPath; "sol.txt"|]
    
    File.WriteAllText(path, sw.ToString())

    let arg = path + " " + solPath

    let res = Util.SystemCallUtil.systemCall oinkPath arg

    let sol = 
        match res with 
        | {ExitCode = 0; Stderr = ""} -> 
            File.ReadAllText(solPath)
            |> parseSolution 
        | {ExitCode = exitCode; Stderr = stderr}  -> 
            if exitCode <> 0 then 
                raise <| ParityGameSolvingException $"Unexpected (non-zero) exit code %i{exitCode}"
            else   
                raise <| ParityGameSolvingException stderr

    let strategy = 
        sol
        |> Map.toList
        |> List.map (fun (k,  (player, m)) -> 
            let n = revd.[k]
            let sucs, _, _ = pg.Properties.[n]

            // If the solution gives no move, we can select any move
            let suc = 
                m 
                |> Option.map (fun x -> revd.[x])
                |> Option.defaultValue (sucs |> Seq.head)
                
            n, (player, suc)
            
            )
        |> Map.ofList


    {
        ParityGameSolutionWithStrategy.WinnerMap = 
            strategy
            |> Map.map (fun _ (a,_) -> a)

        ParityGameSolutionWithStrategy.StrategyMap = 
            strategy
            |> Map.map (fun _ (_, b) -> b)
    }

let solveWithOink intermediateFilesPath oinkPath (pg : ParityGame<'T>) : ParityGameSolution<'T> = 
    let r = solveAndComputeStrategyWithOink intermediateFilesPath oinkPath pg

    {
        ParityGameSolution.WinnerMap = r.WinnerMap
    }

