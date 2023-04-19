module internal ScrabbleBot.boardCalculations

    open ScrabbleUtil
    open Types

    let updateTilesOnBoard (tilesOnBoard: Map<coord, placedTile>) (ms : placedWord) =
        List.fold (fun acc (coord, (_, (cv, pv))) -> Map.add coord (cv, pv) acc) tilesOnBoard ms
        
