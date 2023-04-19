module internal ScrabbleBot.handCalculations
open Types
open MultiSet


    val removePieces : placedWord -> MultiSet<(int * int) * (uint32 * placedTile)> -> MultiSet<(int * int) * (uint32 * placedTile)>
    val addPieces : (uint32 * uint32) list -> MultiSet<uint32> -> MultiSet<uint32>