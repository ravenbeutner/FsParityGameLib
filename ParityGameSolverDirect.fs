module ParityGameLib.ParityGameSolverDirect

open System.Collections.Generic

open Util 
open ParityGame


type private Counter(init : int) =
    let mutable a = init

    new () = Counter(0)

    member this.Reset() =   
        a <- 0

    member this.Get = a

    member this.Inc() =
        a <- a + 1

    member this.Inc(x) =
        a <- a + x
    
    member this.Dec() =
        a <- a - 1

    member this.Dec(x) =
        a <- a - x

let solveDirect (pg : ParityGame<'T>) =
    // Map each states to all edges that have an edge to that state
    let predMap = 
        pg.Properties
        |> Map.toSeq
        |> Seq.map (fun (s, (sucs, _, _)) -> 
            sucs 
            |> Seq.map (fun y -> y, s)
            )
        |> Seq.concat
        |> Seq.groupBy fst 
        |> Seq.map (fun (x, l) -> x, Seq.map snd l |> set)
        |> Map.ofSeq

    // We represent targets as indicator functions to avoids creation of to many sets
    let computeAttractor (allStates : Set<'T>) (target : Set<'T>) (player : ParityGamePlayer) : Set<'T> =     
        let predCounterMap = 
            allStates
            |> Seq.map (fun s -> 
                let sucs, _, _ = pg.Properties.[s]

                let numberOfSuccessors = 
                    sucs 
                    |> Set.intersect allStates 
                    |> Set.count

                s, new Counter(numberOfSuccessors)
                )
            |> Map.ofSeq

        let attractor = new HashSet<'T>(target)
        let queue = new Queue<'T>(target)

        while queue.Count <> 0 do 
            let s = queue.Dequeue() 
            for s' in predMap.[s] do 
                if Set.contains s' allStates then 
                    // Is a state that we care about in the current game
                    let _, p, _ = pg.Properties.[s']

                    if p = player then 
                        if attractor.Contains s' |> not then 
                            attractor.Add s' |> ignore
                            queue.Enqueue s'
                    else
                        // Controlled by the adversary 
                        predCounterMap.[s'].Dec()
                        if predCounterMap.[s'].Get = 0 then
                            // All edges point to winning region so we can add it 
                            if attractor.Contains s' |> not then 
                                attractor.Add s' |> ignore
                                queue.Enqueue s'

        set attractor

    let rec solveRec (currentArea : Set<'T>) = 
        if Set.isEmpty currentArea then 
            [(PlayerZero, Set.empty); (PlayerOne, Set.empty)]
            |> Map.ofList
        else 
            // Compute the max color of all remaining states
            let maxColor = 
                currentArea
                |> Seq.map (fun s ->
                    let _, _, c = pg.Properties.[s]
                    c)
                |> Seq.max

            if maxColor = 0 then 
                // All nodes have color 0, so PlayerZero wins from all states
                [(PlayerZero, currentArea); (PlayerOne, Set.empty)]
                |> Map.ofList
            else 
                let nodesWithMaxColor = 
                    currentArea
                    |> Set.filter (fun s -> 
                        let _, _, c = pg.Properties.[s]
                        c = maxColor
                        )

                let player = if maxColor % 2 = 0 then PlayerZero else PlayerOne 

                let atc = 
                    computeAttractor currentArea nodesWithMaxColor player

                let subgame = Set.difference currentArea atc 
                
                let w = solveRec subgame
                
                if w.[ParityGamePlayer.flip player].Count = 0 then 
                    [   
                        (player, currentArea); 
                        (ParityGamePlayer.flip player, Set.empty)]
                    |> Map.ofList
                else
                    let atc2 = 
                        computeAttractor currentArea w.[ParityGamePlayer.flip player] (ParityGamePlayer.flip player)

                    let subgame2 = Set.difference currentArea atc2

                    let w' = solveRec subgame2

                    let tmp = Set.union w'.[ParityGamePlayer.flip player] atc2
                    
                    [(player, w'.[player]); (ParityGamePlayer.flip player, tmp)]
                    |> Map.ofList

    let res = solveRec (set pg.Properties.Keys)

    {
        ParityGameSolution.WinnerMap = 
            pg.Properties.Keys
            |> Seq.map (fun s -> 
                s, 
                if Set.contains s res.[PlayerZero] then 
                    PlayerZero
                else
                    assert(Set.contains s res.[PlayerOne])
                    PlayerOne
                )
            |> Map.ofSeq
    }

