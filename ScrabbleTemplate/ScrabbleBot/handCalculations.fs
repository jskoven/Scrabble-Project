module internal ScrabbleBot.handCalculations
open Types

    let removePieces (ms : placedWord) hand =
        List.fold (fun acc elem -> MultiSet.removeSingle elem acc) hand ms
 
    let addPieces (np : (uint32 * uint32) list) hand =
        List.fold (fun acc elem -> MultiSet.add (fst elem) (snd elem) acc) hand np
        