module ParityGame 

open System
open System.IO
open System.Collections.Generic

type ParityGamePlayer = 
    | PlayerZero 
    | PlayerOne

module ParityGamePlayer = 
    let asString p = 
        match p with 
        | PlayerZero -> "0"
        | PlayerOne -> "1"

    let flip p = 
        match p with 
        | PlayerZero -> PlayerOne
        | PlayerOne -> PlayerZero

type ParityGame<'T when 'T : comparison> = 
    {
        Properties: Map<'T, Set<'T> * ParityGamePlayer * int>
    }

module ParityGame = 

    let toDotGraphMarked (labels : 'T -> String) (specialNodes : seq<'T>) (pg : ParityGame<'T>) =  
        let nodeToIdDict = 
            pg.Properties.Keys
            |> Seq.mapi (fun i x -> x, "n" + string(i))
            |> Map.ofSeq

        let s = new StringWriter()

        s.WriteLine ("strict digraph test {")
        s.WriteLine ("    forcelabels=true;")

        for n in pg.Properties.Keys do 
            s.WriteLine ("    " + nodeToIdDict.[n] + "[label=" + labels n + "];")

        for entry in pg.Properties do 
            let n = entry.Key
            let sucs, p, c = entry.Value

            match p with 
                | PlayerZero ->
                    s.WriteLine ("    " + nodeToIdDict.[n] + "[shape=box];")
                | PlayerOne -> 
                    ()

            s.WriteLine ("    " + nodeToIdDict.[n] + "[label=\"" + string(c) + "\"];")

            for n' in sucs do
                s.WriteLine ("    " + nodeToIdDict.[n] + " -> " + nodeToIdDict.[n'] + ";")

        for n in specialNodes do 
            s.WriteLine ("    " + nodeToIdDict.[n] + "[fillcolor=yellow, style=filled];")

        s.WriteLine ("}")

        s.ToString()

    let toDotGraph (labels : 'T -> String) (pg: ParityGame<'T>) =  
        toDotGraphMarked labels Seq.empty pg



type ParityGameSolutionWithStrategy<'T when 'T: comparison> = 
    {
        WinnerMap : Map<'T, ParityGamePlayer>
        StrategyMap : Map<'T, 'T>
    }

type ParityGameSolution<'T when 'T: comparison> = 
    {
        WinnerMap : Map<'T, ParityGamePlayer>
    }