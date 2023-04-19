module internal ScrabbleBot.handCalculations
open Types
open MultiSet


    val removePieces : uint32 list -> MultiSet<uint32> -> MultiSet<uint32>
    val addPieces : (uint32 * uint32) list -> MultiSet<uint32> -> MultiSet<uint32>