module internal ScrabbleBot.handCalculations
open Types

    let removePieces (ms : uint32 list) (hand : MultiSet.MultiSet<uint32>) =
        List.fold (fun acc elem -> MultiSet.removeSingle elem acc) hand ms
 
    let addPieces (np : (uint32 * uint32) list) (hand : MultiSet.MultiSet<uint32>) =
        List.fold (fun acc elem -> MultiSet.add (fst elem) (snd elem) acc) hand np
        
        
        //take me out of my misery jesper